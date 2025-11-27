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

## Dependencies

* Use the prerelease stream of the extension
* Have a working Maven repository (private artifacts required)
* Push the artifacts using:
```
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=%DLC%\tty\openedge.core.pl -DgroupId=com.progress.openedge -DartifactId=core -Dversion=12.8.9 -Dpackaging=pl -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=%DLC%\tty\openedge.core.apl -DgroupId=com.progress.openedge -DartifactId=core -Dversion=12.8.9 -Dpackaging=apl -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=%DLC%\tty\netlib\openedge.net.pl -DgroupId=com.progress.openedge -DartifactId=net -Dversion=12.8.9 -Dpackaging=pl -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=%DLC%\tty\netlib\openedge.net.apl -DgroupId=com.progress.openedge -DartifactId=net -Dversion=12.8.9 -Dpackaging=apl -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=..\adecomm.zip -DgroupId=com.progress.openedge -DartifactId=adecomm -Dversion=12.8.9 -Dpackaging=zip -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=..\corelib.zip -DgroupId=com.progress.openedge -DartifactId=core -Dversion=12.8.9 -Dpackaging=zip -Dclassifier=sources -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=..\netlib.zip -DgroupId=com.progress.openedge -DartifactId=net -Dversion=12.8.9 -Dpackaging=zip -Dclassifier=sources -DgeneratePom=false
mvn deploy:deploy-file -Durl=https://nexus.rssw.eu/repository/maven-releases/ -DrepositoryId=rssw -Dfile=..\cefsharp.zip -DgroupId=cefsharp -DartifactId=cefsharp -Dversion=124.3.80 -Dpackaging=zip -Dclassifier=assembly -DgeneratePom=false
```
* Open VS Code!
* Execute "Generate assembly catalog" action
* When completer, restart language server
* Open `debugger01.p` and check compilation status
