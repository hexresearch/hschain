# Configure the Docker provider
provider "docker" {
  host = "unix:///var/run/docker.sock"
}

locals {

  exe_name = "thundermint-tls-coin-node"
  db_path = "tls-coin/db"
  logs_path = "tls-coin/logs"

}

# Create a container
resource "docker_container" "node" {
  count = "${length(var.private_keys)}"
  image = "${docker_image.thundermint.latest}"
  name  = "tls-node-${count.index+1}"
  env   = [ "THUNDERMINT_NODE_SPEC={ \"nspecPrivKey\":\"${var.private_keys[count.index]}\", \"nspecDbName\": \"${local.db_path}/node-${count.index+1}\", \"nspecLogFile\" : [{ \"type\": \"ScribeJSON\", \"path\" : \"${local.logs_path}/node-${count.index+1}\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [${count.index*500},500]}"
          , "THUNDERMINT_KEYS=${jsonencode(values(var.private_keys))}"
          , "KEY_PEM=${var.pems["pk${count.index}"]}"
          , "CERT_PEM=${var.pems["cert${count.index}"]}"
          ]
  command = ["/bin/${local.exe_name}", "--max-h", "10", "--prefix", "/thundermint", "--delay", "100", "--check-consensus", "--deposit", "1000", "--keys", "2000" ]
  volumes = {
    volume_name = "thundermint"
    container_path = "/thundermint"
  }
}

resource "null_resource" "configure-nodes-ips" {

  count = "${length(var.private_keys)}"

  triggers {
    cluster_instance_ids = "${join(",", docker_container.node.*.id)}"
  }

  provisioner "local-exec" {
    command = "bootstrap -t all2all  -l '${jsonencode(docker_container.node.*.ip_address)}' -n ${element(docker_container.node.*.ip_address, count.index)} -p 49999"
  }
}

resource "docker_image" "thundermint" {
  name = "localhost:5000/thundermint-node"
}
