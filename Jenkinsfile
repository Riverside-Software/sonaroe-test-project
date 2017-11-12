#!groovy

stage ('Build') {
  node ('windows') {
    checkout([
      $class: 'GitSCM',
      branches: scm.branches,
      extensions: scm.extensions + [[$class: 'CleanCheckout']],
      userRemoteConfigs: scm.userRemoteConfigs
    ])
    withEnv(["PATH+ANT=${tool name: 'Ant 1.9', type: 'hudson.tasks.Ant$AntInstallation'}/bin", "DLC=${tool name: 'OpenEdge-11.7', type: 'jenkinsci.plugin.openedge.OpenEdgeInstallation'}"]) {
      bat "ant -DDLC=%DLC% -lib Z:\\Tools\\PCT\\PCT-Latest.jar build"
    }
  }
}

stage ('SonarQube') {
  node ('windows') {
    if ('master' == env.BRANCH_NAME) {
      withEnv(["DLC=${tool name: 'OpenEdge-11.7', type: 'jenkinsci.plugin.openedge.OpenEdgeInstallation'}"]) {
        bat "Z:\\Tools\\sonar-scanner-2.8\\bin\\sonar-scanner.bat -Dsonar.host.url=http://sonar.riverside-software.fr -Dproject.settings=sonar-project1.properties -Dsonar.oe.dlc=%DLC%"
      }
    } else {
      withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'GitHub-gquerret', usernameVariable: 'GH_LOGIN', passwordVariable: 'GH_PASSWORD']]) {
        bat "Z:\\Tools\\sonar-scanner-2.8\\bin\\sonar-scanner.bat -Dsonar.host.url=http://sonar.riverside-software.fr -Dproject.settings=sonar-project1.properties -Dsonar.analysis.mode=issues -Dsonar.github.pullRequest=${env.BRANCH_NAME.substring(3)} -Dsonar.github.repository=Riverside-Software/sonaroe-test-project -Dsonar.github.oauth=${env.GH_PASSWORD} -Dsonar.oe.dlc=Z:\\Progress\\OpenEdge-11.7"
      }
    }
  }
}
