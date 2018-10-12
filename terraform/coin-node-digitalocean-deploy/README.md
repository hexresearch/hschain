# Предустановка

1. Установку на DO делает Terraform, основной файл: thundermint-do-docker.tf
1. Необходимо сгенерировать DO-ключ, чтобы Terraform мог с его помощью подключаться к DO API.
1. DO недоступен иногда из РФ, желательно использовать VPN, чтобы установка проходила гладко.
1. В DO надо разместить ssh-ключи для того, чтобы DO мог запускать дроплеты с этим ключами, а Terraform мог с их помощью
   закидывать файлы на дроплеты.
1. Должен быть поднят ssh-agent, чтобы Terraform мог раскидать файлы по дроплетам.
    1. Либо в thundermint-do-docker.tf в секцию `connection` надо явно указать путь к приватному ключу и отключить
       доступ к ssh-agent, например:
    ```
      private_key = "${file("/home/dima/.ssh/id_rsa")}"
      agent = false
    ```
1. Предполагается, что docker-образ должен быть на registry.hxr.team/thundermint-node. Доступ (username/password) можно запросить у Романа.
1. Логи отправляются в elasticsearch по https c basic auth. Поэтому надо иметь учетные данные (username/password) или запросить их у Романа.

# Последовательность действий

1. Собрать контейнер: `nix-build release.nix -A docker-container --max-jobs $(($(nproc) + 1))`. В результате получится
   ссылка `result` на собранный docker-образ.
1. Загрузить его: `docker load < result`.
1. Пометить тегом: `docker tag thundermint-node registry.hxr.team/thundermint-node`.
1. Загрузить в registry: `docker push registry.hxr.team/thundermint-node`.
1. Далее перейти в директорию `terraform/coin-node-digitalocean-deploy`. Поправить и выполнить там `terraform apply`, который
   установит нужные образы на дроплетах. Если были предыдущие развертывания — выполнить `terraform destroy'.
   1. Чтобы Terraform не спрашивал DO-ключ, можно сделать: `export TF_VAR_dotoken=DO-ключ`
1. Все логи доступны в [kibana](https://elastic.hxr.team/kibana).

# Примечания

1. Terraform обычно удаляет и запускает ноды снова (destroy/apply). Поэтому они каждый раз имеют разные ip-адреса.
1. Чтобы посмотреть текущие ip-адреса cохраняются в файле `inventory`.
1. Чтобы ноды работали не через NAT, можно `-p 49999-50000:49999-50000` заменить на `--network host` в tf-файле.
1. Сейчас ноды запускаются без поддержки IPv4. Можно включить поддержку IPv6 следующим образом:
   https://www.terraform.io/docs/providers/do/r/droplet.html#ipv6 :
   1. На одном уровне с image, size добавить `ipv6=true`.
