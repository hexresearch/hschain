# Configure the Docker provider
provider "docker" {
  host = "unix:///var/run/docker.sock"
}

variable "private_keys" {
  default = {
    "0" = "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
    "1" = "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
    "2" = "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
  #  "3" = "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  #  "4" = "6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp"
  #  "5" = "7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk"
  #  "6" = "7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN"
  }
}

# Create a container
resource "docker_container" "node" {
  count = "${length(var.private_keys)}"
  image = "${docker_image.scratch.latest}"
  name  = "node-${count.index+1}"
  env   = [ "THUNDERMINT_NODE_KEY=${var.private_keys[count.index]}"
          , "THUNDERMINT_KEYS=${jsonencode(values(var.private_keys))}"
          ]
  volumes = {
    volume_name = "logs"
    container_path = "/logs"
  }
}

resource "null_resource" "configure-nodes-ips" {

  count = "${length(var.private_keys)}"

  triggers {
    cluster_instance_ids = "${join(",", docker_container.node.*.id)}"
  }

  provisioner "local-exec" {
    command = "echo '${jsonencode(docker_container.node.*.ip_address)}' | netcat -v ${element(docker_container.node.*.ip_address, count.index)} 49999"
  }
}

resource "docker_image" "scratch" {
  name = "localhost:5000/thundermint-node"
}

