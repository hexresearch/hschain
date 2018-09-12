# Configure the Docker provider
provider "docker" {
  host = "unix:///var/run/docker.sock"
}

variable "private_keys" {
  default = {
    "0" = "2K7bFuJXxKf5LqogvVRQjms2W26ZrjpvUjo5LdvPFa5Y"
    "1" = "4NSWtMsEPgfTK25tCPWqNzVVze1dgMwcUFwS5WkSpjJL"
    "2" = "3Fj8bZjKc53F2a87sQaFkrDas2d9gjzK57FmQwnNnSHS"
    "3" = "D2fpHM1JA8trshiUW8XPvspsapUvPqVzSofaK1MGRySd"
  #  "4" = "6KpMDioUKSSCat18sdmjX7gvCNMGKBxf7wN8ZFAKBvvp"
  #  "5" = "7KwrSxsYYgJ1ZcLSmZ9neR8GiZBCZp1C1XBuC41MdiXk"
  #  "6" = "7thxDUPcx7AxDcz4eSehLezXGmRFkfwjeNUz9VUK6uyN"
   # "7" = "BrZwNdLi2KmZrrChNPrso8AhxxhHruNT3c1nmBsFbepA"
   # "8" = "ApxvqeTq39enZcbtFK7ERdft4i9xtnUMYW477mHSJttA"
   # "9" = "otXaib9kazAmU5wmbd8NVMe3RF13GCXsbDZkR8yJvGU"
   #"10" = "AYCBeEAArtpBiQrUQc43UnrYKhyu4ivLg9sHuUtS6yBd"
   #"11" = "1Zx4nfaRL6hpYpLSgjW8tEoTj9pgQxHqFuAoi1Jm72Q"
   #"12" = "qdYSqU24VsEQTZL6ECXnxP9gvv9CUugQxtKaVHQks9y"
   #"13" = "5Yxu5cdZ2qDkWR98mdDDo6YgPUAhLqSyn8tUVVHQFSv8"
   #"14" = "2JFakzbrqCLm5iWNrMrHcQ1SAq6fQgnqYaRqxYn7yiuU"
   #"15" = "HQP6B1B92yVr6XR6SWsuZG25qj62iNkPpQ6Pu1JXrY3g"
   #"16" = "E2XpmEZdAb7VfrisCxkzwy74sVgJ4g9KzLUDa819NQaK"
   #"17" = "EkSUKapiH5g29S83PaH4cKk8mhYqkTfMpiHHb4T7A1GH"
   #"18" = "oV8sYb8pLo8FDVtEkqUeW62vB5ZLAb5awZYNdrCQSHy"
   #"19" = "CRPX1zwP9uiUCB57FED82bj91qkn1jfPfm372WfDrHLp"
   #"20" = "3ybWJ1cqMK5Z1mEUHxgz8GWuzjW6mAP87sdSQED7mXYM"
   #"21" = "2GW5RA3szshnBnbWS9Rhk2XLpvUD9SGdbvgoRTaJboS5"
   #"22" = "CxU1kaUfFJMykr6VMDvRAFNKXADy3qgZK18g2npNpmpu"
   #"23" = "FU7EBYsKGTwFekvcYipJS6ubq6jCj95QiB7UfjmqCN8X"
   #"24" = "7XzhwYEgNVh63pJapQpQg8AUFNWVMyBZjp7YNVWXcNPA"
   #"25" = "4fivLuFYt49yee1A5fAp8fFwyUwxCumy9oxtsFqeoieS"
   #"26" = "DvbMJyCNXRamfQiFwpHZ2xhGThJbVL7gWVgvcce7YQaY"
   #"27" = "37Ra1gSktFMy9bQwfewRqBxFGyQQHs9pECyirw17tBZg"
   #"28" = "5zzYmvu4VE1VNLQdkZtSDCE2V1pU6HdGXBmKYPMNkDxF"
   #"29" = "8u66TLeBkw2Vg1zVoCMDGXHkgoJqAaMREUYstQjVAQxY"
   #"30" = "Ff7LjPDKcvJnzMVSuUTUxFenRoUNLXmsCV7thn3JB8HT"
   #"31" = "Bwxm5BnD41Zcyk9n46BwBuseyfZrqcB8vWF5DXkwWZm"
   #"32" = "4SZ2sf2SykzWWsmeQvrgLWhypdEH3dv5iUR8thESnAjA"
   #"33" = "HVKH7uFb23nYSBn7Qbs98zDQX9etZghiu1C8yR8iisCH"
   #"34" = "F3ARgLffrVVooArYxHZsj3w7xk98enBonEnvtZ23DMAH"
   #"35" = "FNeM4o4uPKKpKXTqz2JTcnXDf2phNk8rH9QtSXJKieqJ"
   #"36" = "GgiTZEmUhD7eiDKSJFpyjZfSd4t32FSrkLAvY25dy8j8"
   #"37" = "7Lc6P7d1rg2ExVewYcBPtY2QPoSo8JYC4GhQb149v1bg"
   #"38" = "A7DtjtkgpjdfYw3PEi4asw21anrSTXyS1VDA1dq55HJd"
   #"39" = "AoXL23rGCcTDq3XKaG85KWMgRxuBogSbpCP1WzmAcTNR"
   #"40" = "F4uNnwFiTkTCcFbNqGiC5Hxi46ZSHekww3rX865UUnLS"
   #"41" = "GAM3M9MngZALNLx4qizeS6MunjcmsEgj6rRwjQtZjyjT"
   #"42" = "91aJK5ZUWEAtPiweif6XuQfUoQvumK6kxmzsd5J3UzPP"
   #"43" = "DvmvR4J8zWixMatbLgCbkMUaKRbyNNcXC1BwHDhgkBfj"
   #"44" = "GbjRieFirGqDmRMNJm7D7pxXFqc75WnmdvbiUM7jRLjJ"
   #"45" = "Hpo1Y9F3ivGFv7rwweSrgU7RfZHoQ25L9wA4SuVkdUPq"
   #"46" = "7TvafqPGK8RNYYySAnzYMwC5EhzSi4APUzYEGxMdQ2o1"
   #"47" = "2btVKLiUa8Tj2tQbehk4SoNoWgkeUBrRhXmqg8eERLVL"
   #"48" = "5RJ2yJFn3jpyEagoJybtpXZu9SP5coQHf9qkmEKqQ6Sr"
   #"49" = "8aXuXAYtZ4eQy7upzmek8UGekiuny4wicr9jJuQsgUBC"
   #"50" = "9GC457i2RLhf2rSCkwjSw5PJbMHmz5NSaHmx6WUJX9FH"
   #"51" = "cwGwthm1pUci1Wwutvgi2aNG4yKK1Z6NwyTyEEGnXft"
   #"52" = "GL1fWGc4kN1MASswBQphKnf1rHmnyCHT8NF8UH5NdNdS"
   #"53" = "7jrASaeTePGirHRWVva4PGKm2aqe4PPcphgKDoQYS2sj"
   #"54" = "2w8BuKJpkirpgtxtqFepe1U6MocVSSEgKYC4evWqfGar"
   #"55" = "E4ibBWMHCTG58Zqf1zmZYHk5mRWbdW9eX8ugjNnMtUUx"
   #"56" = "bHHkTHXcU2EM5A83b2ttCEabKCubXFi4XYia3quGXLJ"
   #"57" = "8cZfufWj5ZE2xkzAAEtPaJ56VVDzcwpC4zvbZH1Ai3u9"
   #"58" = "HkDV4ALcW1V29Uqo82xpZQt6iwqjiZhnkJDPRqzVRC3t"
   #"59" = "5h55cQGMgyCfFMHcbsDK7hpTVrpEGWhvDtar66JwWAvv"
   #"60" = "8hEjZXB9cpPjgwUfrq67MQohDW5drj1gw7CWguV2cU8P"
   #"61" = "9KkEQ71fP2qB3TiBmBAWM22EXHY2H48CakxTE83V3BXp"
   #"62" = "G6Ue83qLwEqozRih9k6SuPVcvAy4xef1BKXpov6iJWYm"
   #"63" = "3gxhSeN9L9QNkdT5qUJcE4LBJ6wyLK7uEKpjsqP8wvGZ"
   #"64" = "7agAPAzMdX3D6BxqYZB8rfJTiy59uzp2yqCa6szLMbHb"
   #"65" = "DGQqKpJU2wMcPvfyyxR1YwZL85D4rGXYzmHjVC6TPm1e"
   #"66" = "CrjwwiYJeVsTy1y2wWMwKeP5JhXqGVeX7UpQ3XHw12dH"
   #"67" = "6WysanaGLz1hCUh9ZsfyR96kGYmWcFZ2gxV8n8Dx1YrR"
   #"68" = "EpoajsAR9voZ4in8hiPhUgAsbbEbu7JAgezipD23bv9X"
   #"69" = "eNVtWYgPZPQypTYtVoFwgkXzVbg8QcfiE1bv6giekSX"
   #"70" = "EiqteyyiJafrsCwSiwzgnF7PbPYxVNEcdgp5wXFRPYvr"
   #"71" = "CNsp5HYTLiv5AnwDycGY4yj1kJGsEFDHCLh5Rj1ZDQfX"
   #"72" = "931zpKpEW3oCk1ZtjQPmK9e8HG1fc7gdHyFkSXrnmyHZ"
   #"73" = "2sRkdTRjSCBnfqwUxk5adopg5sLZrXV5sruQ2bV84Mtk"
   #"74" = "EsJfjabZqrXwPsEabS41ekU2hvVB72NNB4ZAYpLGVRhU"
   #"75" = "CZ9b5YjvCqWLbJ1jmfDtNa4GnUPsf8hQLPXEWVR8nCe2"
   #"76" = "3tX8ZuLNT9TVYUtyqoPqujajdxMD8r3XKxap9PqJRv66"
   #"77" = "Ez79RAaeezSVMCJm2wTc3JqWEJS57oyuX8AzPxWuVaob"
   #"78" = "CXuESUH787vmuM28urhpKedYdYAcujmKWLeuqoeUE5nA"
   #"79" = "2WU7FpiRFY4cyc9bj3Jp7AXUgZexNeZfWuHkYBDNvsTc"
   #"80" = "5hSpDPH7zEJf3dKT5tLQ7pii1KujddE8PCPrTSD5BDdk"
   #"81" = "GMoHqCkM2uqAW8eF5iJ53J3GYFgZvuQW63yD3Pg3tEms"
   #"82" = "24CykCnTDzmdNt7bEC4F9KHERkzFnpFFGYwoFEHMsGjU"
   #"83" = "ECMck2K9YrWtRtoisRqBkrgC6UAe4c2esYvXLsMTWuBB"
   #"84" = "6TXH2FXmYGcnKgW3jFv9evhxnGExFKTZQ8aCg5GfDtx3"
   #"85" = "6ZUPw5jQd8MaLjTTGc7A1PbByXYrfnG2FRywAPPP9vny"
   #"86" = "E5TGmv3YXjj3b23RGHTe3RkjSsgT3opsvy2dfNt1WjJL"
   #"87" = "2KDKYCY58yjH4BAftaiom6pVvKBEAtxqYk3byoKP9MSo"
   #"88" = "BdouGMj1hxrgnbXA3akMBYNcfV7wLiiXTy7vU9BiT7H"
   #"89" = "29sFUUPmMi7mp3D6yUGN3PotYpcirgfYkCvFjWnMX3w5"
   #"90" = "57KrUCwBDBNwLeewbsrpEfBqy7mMNNmXqudmSi14b57W"
   #"91" = "AfLasRN2xaxtGSrofUSbCSpztwTKnsDuFsZKd5Eukh24"
   #"92" = "A2fz4a3oWMi6pY6Y3UXNcLnvNJQn2sEQgw51ejAPkUXP"
   #"93" = "99j7FT31KxNhZGcAR19ErfPwSRnEorJg72kWP1MhAedX"
   #"94" = "CHevu7B9omMuWVNJAxh3bMUghYrNQuF1TCfKMQQCikn9"
   #"95" = "91dEa9Dxe1dgDyZF2N1hbsZmxYgJw8ygKugjrVSZLMqy"
   #"96" = "9LhyU1yV8gnCsBjXAu2paFZ4jLYRn5ksLqVh5yvaAmRh"
   #"97" = "81PgsBkUBRr3JedBjCTos9df5o1JuEtpZ98ejmij6fik"
   #"98" = "2AoPruXq98K6p6QBZbFxqFEtUqcD5SeXennsstEABGht"
   #"99" = "6ZDsniHN2YRcSGpG6QzgbuRvCQD7mQduezg2TWvQTKBK"
   #"100" = "BF5DvE9jydC63vSFRphDTsv2VBzY2C28tUewgZic4Xcz"
  }
}

# Create a container
resource "docker_container" "node" {
  count = "${length(var.private_keys)}"
  image = "${docker_image.scratch.latest}"
  name  = "node-${count.index+1}"
  env   = [ "THUNDERMINT_NODE_SPEC={ \"nspecPrivKey\":\"${var.private_keys[count.index]}\", \"nspecDbName\": \"db/node-${count.index+1}\", \"nspecLogFile\" : [{ \"type\": \"ScribeJSON\", \"path\" : \"logs/node-${count.index+1}\", \"severity\" : \"Debug\", \"verbosity\" : \"V2\" }], \"nspecWalletKeys\"  : [${count.index*500},500]}"
          , "THUNDERMINT_KEYS=${jsonencode(values(var.private_keys))}"
          ]
  command = ["/bin/thundermint-coin-node", "--max-h", "10", "--prefix", "/thundermint", "--delay", "100", "--check-consensus", "--deposit", "1000", "--keys", "2000" ]
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

resource "docker_image" "scratch" {
  name = "localhost:5000/thundermint-node"
}
