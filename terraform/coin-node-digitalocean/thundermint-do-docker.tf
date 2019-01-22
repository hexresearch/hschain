variable "dotoken" {}

variable "elastic_url" {
  default = "elastic.hxr.team"
}

variable "registry_url" {
  default = "registry.hxr.team"
}

variable "elastic_user" {
  default = "elastic"
}

variable "registry_user" {
  default = "romcheck"
}

variable "elastic_pass" {}

variable "registry_pass" {}

provider "digitalocean" {
  token = "${var.dotoken}"
}

variable "cluster-id" {
  default = "coin"
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
  size     = "s-3vcpu-1gb"
#  region   = "${element(var.regions, count.index)}"
  region   = "fra1"
  name     = "node${count.index+1}"
  ssh_keys = ["22607679"]
  private_networking = true
}

data "template_file" "inventory" {
  count = "${length(var.private_keys)}"
  template = "$${inventory_hostname} ansible_host=$${ansible_host} private_host=$${private_host}"
  vars {
    inventory_hostname = "${var.cluster-id}-node${count.index + 1}"
    ansible_host = "${element(digitalocean_droplet.node.*.ipv4_address,count.index)}"
    private_host = "${element(digitalocean_droplet.node.*.ipv4_address_private,count.index)}"
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
      "docker login -v -u ${var.registry_user} -p ${var.registry_pass} ${var.registry_url}",
      "docker run --name node${count.index+1} -d --net host -e THUNDERMINT_KEYS='${jsonencode(values(var.private_keys))}' -e THUNDERMINT_NODE_SPEC='{ \"nspecPrivKey\":\"${var.private_keys[count.index]}\", \"nspecLogFile\" : [{ \"type\" : { \"tag\" : \"ScribeES\", \"index\" : \"thundermint\" }, \"path\" : \"https://${var.elastic_user}:${var.elastic_pass}@${var.elastic_url}\", \"severity\" : \"Info\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [${count.index*4},4]}' registry.hxr.team/thundermint --max-h 10000 --delay 10 --deposit 1000 --keys 16 --peers '${jsonencode(digitalocean_droplet.node.*.ipv4_address)}' --node-n ${count.index+1} --total-nodes ${length(var.private_keys)}"
    ]
  }
}

resource "null_resource" "node" {
  triggers {
    cluster_instance_validator_ids = "${join(",", digitalocean_droplet.node.*.id)}"
  }
  provisioner "local-exec" {
    command = "echo -e '[${var.cluster-id}:children]\n${var.cluster-id}-node' > inventory"
  }
  provisioner "local-exec" {
    command = "echo -e '[${var.cluster-id}-node]\n${join("\n",data.template_file.inventory.*.rendered)}' >> inventory"
  }
}
