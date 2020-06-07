### Запуск тестов производительности
Требования: установленный sbt.
```shell script
bash sbt "core/test:runMain benchmarking.script.FullScript"
```
### Публикация docker-образов
Требования: установленный sbt.
```shell script
bash deployment/publish.sh
```
### Развертывание и обновление системы
Требования: команда выполняется с узла-лидера docker swarm 
```shell script
bash deployment/deploy.sh
```
### Свертывание системы
```shell script
bash deployment/shutdown.sh
```