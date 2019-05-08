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
    withEnv(["DLC=${tool name: 'OpenEdge-11.7', type: 'jenkinsci.plugin.openedge.OpenEdgeInstallation'}"]) {
      withCredentials([string(credentialsId: 'AdminTokenSonarQube', variable: 'SQ_TOKEN')]) {
        if ('master' == env.BRANCH_NAME) {
          bat "Z:\\Tools\\sonar-scanner-2.8\\bin\\sonar-scanner.bat -Dsonar.host.url=http://sonar.riverside-software.fr  -Dsonar.login=${env.SQ_TOKEN} -Dsonar.branch.name=${env.BRANCH_NAME} -Dsonar.oe.dlc=%DLC% -X"
        } else {
          bat "Z:\\Tools\\sonar-scanner-2.8\\bin\\sonar-scanner.bat -Dsonar.host.url=http://sonar.riverside-software.fr  -Dsonar.login=${env.SQ_TOKEN} -Dsonar.branch.name=${env.BRANCH_NAME} -Dsonar.branch.target=master -Dsonar.oe.dlc=%DLC%"
        }
      }
    }
  }
}
