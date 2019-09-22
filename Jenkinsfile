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
    withEnv(["PATH+SCAN=${tool name: 'SQScanner4', type: 'hudson.plugins.sonar.SonarRunnerInstallation'}/bin", "DLC=${tool name: 'OpenEdge-11.7', type: 'openedge'}"]) {
      withSonarQubeEnv('RSSW') {
        if ('master' == env.BRANCH_NAME) {
          bat "sonar-scanner -Dsonar.oe.dlc=%DLC% -Dsonar.branch.name=%BRANCH_NAME%"
        } else {
          bat "sonar-scanner -Dsonar.oe.dlc=%DLC% -Dsonar.branch.name=%BRANCH_NAME% -Dsonar.branch.target=master"
        }
      }
    }
  }
}
