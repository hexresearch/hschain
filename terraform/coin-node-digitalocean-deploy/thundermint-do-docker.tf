variable "dotoken" {}
variable "elasticpass" {}

provider "digitalocean" {
  token = "${var.dotoken}"
}

variable "private_keys" {
  default = {
    "0" = "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
    "1" = "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
    "2" = "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
    "3" = "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  }
}

variable "regions" {
  default = ["fra1", "lon1", "sgp1", "ams3"]
}

resource "digitalocean_droplet" "node" {

  count    = "${length(var.private_keys)}"
  image    = "docker-18-04"
  size     = "4gb"
  region   = "${element(var.regions, count.index)}"
  name     = "node-${count.index+1}"
  ssh_keys = ["22607679", "22838852", "22986074"]

  provisioner "file" {

    content = "input { gelf {} } output { elasticsearch { hosts => ['https://elastic.hxr.team:443'] user => 'elastic' password => '${var.elasticpass}' } }"
    destination = "/opt/logstash.conf"

    connection {
      type    = "ssh"
      timeout = "1m"
    }

  }

  provisioner "remote-exec" {

    connection {
      type         = "ssh"
      timeout      = "1m"
      #private_key = "${file("~/.ssh/id_rsa")}"
      #agent       = false
    }

    inline = [
      "docker run -p 172.17.0.1:12201:12201/udp -d -v /opt/logstash.conf:/logstash.conf docker.elastic.co/logstash/logstash-oss:6.4.2 -f /logstash.conf",
      "docker volume create thundermint",
      "docker run --log-driver gelf --log-opt gelf-address=udp://172.17.0.1:12201 -v thundermint:/thundermint -p 49999-50000:49999-50000 --name node-${count.index+1} -d -e THUNDERMINT_KEYS='${jsonencode(values(var.private_keys))}' -e THUNDERMINT_NODE_SPEC='{ \"nspecPrivKey\":\"${var.private_keys[count.index]}\", \"nspecDbName\": \"db/node-${count.index+1}\", \"nspecLogFile\" : [{ \"type\": \"ScribeJSON\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [${count.index*500},500]}' dmalkr/thundermint-node /bin/thundermint-coin-node --prefix /thundermint --delay 100 --check-consensus --deposit 1000 --keys 2000"
    ]

  }

}

resource "null_resource" "configure-nodes-ips" {

  count = "${length(var.private_keys)}"

  triggers {
    cluster_instance_ids = "${join(",", digitalocean_droplet.node.*.id)}"
  }

  provisioner "local-exec" {
    command = "bootstrap -t all2all  -l '${jsonencode(digitalocean_droplet.node.*.ipv4_address)}' -n ${element(digitalocean_droplet.node.*.ipv4_address, count.index)} -p 49999"
  }

}
