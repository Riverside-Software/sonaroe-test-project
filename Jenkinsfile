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
        checkout([$class: 'GitSCM', branches: scm.branches, extensions: scm.extensions + [[$class: 'CleanCheckout']], userRemoteConfigs: [[credentialsId: scm.userRemoteConfigs.credentialsId[0], url: scm.userRemoteConfigs.url[0], refspec: '+refs/heads/main:refs/remotes/origin/main']] ])

        withEnv(["DLC=${tool name: 'OpenEdge-12.2', type: 'openedge'}", "TERM=xterm"]) {
          sh "$DLC/ant/bin/ant -DDLC=$DLC -lib $DLC/pct/PCT.jar -lib xmltask.jar build"
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
                sh "sonar-scanner -Dsonar.oe.dlc=$DLC -Dsonar.pullrequest.branch=$BRANCH_NAME -Dsonar.pullrequest.base=main -Dsonar.pullrequest.key=$BRANCH_NAME"
              }
            }
          }
        }
      }
    }

  }
}

