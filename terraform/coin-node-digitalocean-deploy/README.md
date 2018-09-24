# Предустановка

1. Установку на DO делает Terraform, основной файл: thundermint-do-docker.tf
1. Ещё можно сделать выкладку логов с дроплетов на S3 с помощью Ansible, но я по этому поводу с Ромой ещё не общался и
   сам про Ansible не знаю. Все yml-файлы -- это файлы ansible. TODO уточнить про Ansible. 
1. DO недоступен иногда из РФ, желательно использовать VPN, чтобы установка проходила гладко.
1. В DO надо разместить ssh-ключи для того, чтобы DO мог запускать дроплеты с этим ключами, а Terraform мог с их помощью
   закидывать файлы на дроплеты.
1. Необходимо сгенерировать DO-ключ, чтобы Terraform мог с его помощью подключаться к DO API.
1. Должен быть поднят ssh-agent, чтобы Terraform мог раскидать файлы по дроплетам.
    1. Либо в thundermint-do-docker.tf в секцию `connection` надо явно указать путь к приватному ключу и отключить
       доступ к ssh-agent, например:
    ```
      private_key = "${file("/home/dima/.ssh/id_rsa")}"
      agent = false
    ```
1. Предполагается, что docker-образ должен быть доступен с hub.docker.com, поэтому надо сделать там учётку (дальше
   предполагается, что она называется `myusername`), затем сделать публичный образ `thudermint-node`, после чего
   поменять в tf-файле 'romcheck/thundermint-node' на 'myusername/thundermint-node'. Затем залогиниться локально:
   `docker login`.

# Последовательность действий

1. Собрать контейнер: `nix-build release.nix -A docker-container --max-jobs $(($(nproc) + 1))`. В результате получится
   ссылка `result` на собранный docker-образ.
1. Загрузить его: `docker load < result`.
1. Пометить тегом: `docker tag thundermint-node:latest myusername/thundermint-node`.
1. Загрузить на свой хаб: `docker push myusername/thundermint-node`.
1. Можно зайти на hub.docker.com и проверить, что образ обновился.
1. Далее перейти в директорию `terraform/coin-node-digitalocean-deploy` и выполнить там `terraform apply`, который
   установит нужные образы на дроплетах. Возможно, перед этим надо будет сделать
   `terraform destroy`, чтобы удалить предыдущие ноды (если Terraform ещё не сделал .state-файл).
   1. Чтобы Terraform не спрашивал DO-ключ, можно сделать: `export TF_VAR_dotoken=DO-ключ`
1. Для завершения работы: `terraform destroy`.

# Примечания

1. Terraform обычно удаляет и запускает ноды снова. Поэтому они каждый раз имеют разные IP.
    1. Хотя Рома говорит, можно и переделать скрипт, чтобы ноды были такими же TODO: уточнить.
1. Чтобы посмотреть текущие ip-адреса надо выполнить в директории `terraform/coin-node-digitalocean-deploy`:
   `terraform show |grep ipv4_address| awk '{print $3}'`.
1. Чтобы ноды работали не через NAT, можно `-p 49999-50000:49999-50000` заменить на `--network host` в tf-файле.
