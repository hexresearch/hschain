# Ноутбуки для запросов к эластику

Ноутбуки — это альтернативный кибане способ добывать информацию из эластика.
Все необходимые для запуска пакеты поттягиваются при помощи никса.  Запускать
сервер можно скриптом `sh run-jupyter.sh`.

Для того чтобы не хранить логин-пароль в git, они вынесены в отдельный файл
`credentials.json` вида:

```
{
  "login"    : "login",
  "password" : "password"
}
```

Для запросов к эластику можно использовать ноутбук `elastic_queries.ipynb`, но
может быть предпочтительно скопировать его чтобы не напороться на конфликты с гитом.

