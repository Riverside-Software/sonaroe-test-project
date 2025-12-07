# Sample project for CABL (OpenEdge plugin for SonarQube) and VS Code

## Requirements

* OpenEdge 12.8 on Windows or Linux

## Configuration

* Clone project in your local directory
* Create local database with `%DLC%\ant\bin\ant -lib xmltask.jar -lib %DLC%\pct\PCT.jar db`
* Open project in VS Code

## Build with Ant / PCT

* Execute `%DLC%\ant\bin\ant build`

## Execute sonar-scanner

* Download [sonar-scanner](https://docs.sonarsource.com/sonarqube-server/analyzing-source-code/scanners/sonarscanner)
* Execute `sonar-scanner -Dproject.settings=src/dev/sonar/sonar-project.properties`
