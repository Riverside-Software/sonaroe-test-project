pipeline {
  agent none
  options {
    disableConcurrentBuilds()
    buildDiscarder(logRotator(numToKeepStr: '10'))
    timeout(time: 60, unit: 'MINUTES')
    skipDefaultCheckout()
  }

  stages {
    stage ('Build') {
      agent { label 'linux' }
      steps {
        checkout([$class: 'GitSCM', branches: scm.branches, extensions: scm.extensions + [[$class: 'CleanCheckout']], userRemoteConfigs: scm.userRemoteConfigs])
        withEnv(["PATH+ANT=${tool name: 'Ant 1.9', type: 'hudson.tasks.Ant$AntInstallation'}/bin", "DLC=${tool name: 'OpenEdge-12.2', type: 'jenkinsci.plugin.openedge.OpenEdgeInstallation'}", "TERM=xterm"]) {
          sh "ant -DDLC=$DLC -lib PCT.jar -lib xmltask.jar build"
        }
      }
    }

    stage ('SonarQube') {
      agent { label 'linux' }
      steps {
        withEnv(["PATH+SCAN=${tool name: 'SQScanner4', type: 'hudson.plugins.sonar.SonarRunnerInstallation'}/bin", "DLC=${tool name: 'OpenEdge-12.2', type: 'openedge'}"]) {
          withSonarQubeEnv('RSSW') {
            script {
              if ('main' == env.BRANCH_NAME) {
                sh "sonar-scanner -Dsonar.oe.dlc=$DLC -Dsonar.branch.name=$BRANCH_NAME"
              } else {
                sh "sonar-scanner -Dsonar.oe.dlc=$DLC -Dsonar.branch.name=$BRANCH_NAME -Dsonar.branch.target=main"
              }
            }
          }
        }
      }
    }

  }
}

