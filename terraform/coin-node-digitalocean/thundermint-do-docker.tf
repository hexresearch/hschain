variable "dotoken" {}

variable "elastic_url" {
  default = "elastic.hxr.team"
}

variable "registry_url" {
  default = "registry.hxr.team"
}

variable "auth_user" {
  default = "elastic"
}

variable "tag" {
  default = "latest"
}

variable "port" {
  default = "50000"
}

variable "auth_pass" {}

provider "digitalocean" {
  token = "${var.dotoken}"
}

variable "cluster-id" {
  default = "coin"
}

variable "private_keys" {
  default = {
    "0" = "Vv36tyN7Vw8FTqzcVhhz3zyyqiC5bD2Q7fF4CpSzSVF"
    "1" = "8zy9cdTaqwzRkjjNa2ngsBFkiePbjkeXBJW3uNgrCsH1"
    "2" = "FPXWML5Z4tq2qq4dHtnsMV22TaFUTY8U6PNGMzGUbyA"
    "3" = "3qL9i1gLgpXKFt4i54QaTQofk6fU8XirQteYbWzcQLVB"
    "4" = "229KaJdRBdxAgmkPUbpKAez2L3cNAQDyQkDLG5zz8md4"
    "5" = "9VauHFBipRepsZXroYn99kM8eXEi2WwT3D6rehsgxhpp"
    "6" = "Dxgq4sghFUkGeiRKcARGGsvav6D82NMLegmxLyCa4Udt"
    "7" = "Fa3KRZYK6zEPqyscd1wgVij73YXhXepkQVvz9hXMogU7"
    "8" = "DtNQTEbCsqoow5W1Z7SRDoTCd6Fpur5yr7zxQbjDkbHa"
    "9" = "QhbeeT9ia7N5JJ6iWJh8hLZ9ThnRwqHb85eBu2g5HKZ"
  }
}

variable "regions" {
  default = ["fra1", "lon1", "sgp1", "ams3"]
}

resource "digitalocean_droplet" "node" {
  count    = "${length(var.private_keys)}"
  image    = "docker-18-04"
  size     = "s-3vcpu-1gb"
  #region   = "${element(var.regions, count.index)}"
  region   = "fra1"
  name     = "${var.cluster-id}-node${count.index+1}"
  ssh_keys = [22607679, 23844768]
  #private_networking = true
}

data "template_file" "inventory" {
  count = "${length(var.private_keys)}"
  template = "$${inventory_hostname} ansible_host=$${ansible_host}"
  #template = "$${inventory_hostname} ansible_host=$${ansible_host} private_host=$${private_host}"
  vars {
    inventory_hostname = "${var.cluster-id}-node${count.index + 1}"
    ansible_host = "${element(digitalocean_droplet.node.*.ipv4_address,count.index)}"
    #private_host = "${element(digitalocean_droplet.node.*.ipv4_address_private,count.index)}"
  }
}

resource "null_resource" "deploy" {
  triggers {
    cluster_instance_xenochain_ids = "${join(",", digitalocean_droplet.node.*.id)}"
  }
  count    = "${length(var.private_keys)}"

  provisioner "remote-exec" {

    connection {
      timeout      = "1m"
      host         = "${element(digitalocean_droplet.node.*.ipv4_address,count.index)}"
    }

    inline = [
      "systemctl stop ufw",
      "systemctl disable ufw",
      "docker login -u ${var.auth_user} -p ${var.auth_pass} ${var.registry_url}",
      "docker run --restart always --name ${var.cluster-id}-node${count.index+1} -d --net host -e THUNDERMINT_KEYS='${jsonencode(values(var.private_keys))}' -e THUNDERMINT_NODE_SPEC='{ \"nspecPrivKey\":\"${var.private_keys[count.index]}\", \"nspecLogFile\" : [{ \"type\" : { \"tag\" : \"ScribeES\", \"index\" : \"thundermint\" }, \"path\" : \"https://${var.auth_user}:${var.auth_pass}@${var.elastic_url}\", \"severity\" : \"Info\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [${count.index*4},4]}' registry.hxr.team/thundermint:${var.tag} --max-h 10000 --delay 10 --deposit 1000 --keys 16 --peers '${jsonencode(formatlist("%s:%s", digitalocean_droplet.node.*.ipv4_address, var.port))}' --node-n ${count.index} --total-nodes ${length(var.private_keys)}"
    ]
  }
}

output "inventory" {
  value = "\n${join("\n",data.template_file.inventory.*.rendered)}"
}
