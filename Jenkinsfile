#!groovy

stage ('Build') {
  node ('linux') {
    checkout([
      $class: 'GitSCM',
      branches: scm.branches,
      extensions: scm.extensions + [[$class: 'CleanCheckout']],
      userRemoteConfigs: scm.userRemoteConfigs
    ])
    withEnv(["PATH+ANT=${tool name: 'Ant 1.9', type: 'hudson.tasks.Ant$AntInstallation'}/bin", "DLC=${tool name: 'OpenEdge-12.2', type: 'jenkinsci.plugin.openedge.OpenEdgeInstallation'}", "TERM=xterm"]) {
      sh "ant -DDLC=$DLC -lib PCT.jar -lib xmltask.jar build"
    }
  }
}

stage ('SonarQube') {
  node ('linux') {
    withEnv(["PATH+SCAN=${tool name: 'SQScanner4', type: 'hudson.plugins.sonar.SonarRunnerInstallation'}/bin", "DLC=${tool name: 'OpenEdge-12.2', type: 'openedge'}"]) {
      withSonarQubeEnv('RSSW') {
        if ('master' == env.BRANCH_NAME) {
          sh "sonar-scanner -Dsonar.oe.dlc=$DLC -Dsonar.branch.name=$BRANCH_NAME"
        } else {
          sh "sonar-scanner -Dsonar.oe.dlc=$DLC -Dsonar.branch.name=$BRANCH_NAME -Dsonar.branch.target=master"
        }
      }
    }
  }
}
