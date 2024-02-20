pipeline {
  agent none
  options {
    disableConcurrentBuilds()
    buildDiscarder(logRotator(numToKeepStr: '10'))
    timeout(time: 10, unit: 'MINUTES')
    skipDefaultCheckout()
  }

  stages {
    stage ('Build') {
      agent { label 'Linux-Office03' }
      steps {
        checkout([$class: 'GitSCM', branches: scm.branches, extensions: scm.extensions + [[$class: 'CleanCheckout']], userRemoteConfigs: scm.userRemoteConfigs])
        checkout([$class: 'GitSCM', branches: scm.branches, extensions: scm.extensions + [[$class: 'CleanCheckout']], userRemoteConfigs: [[credentialsId: scm.userRemoteConfigs.credentialsId[0], url: scm.userRemoteConfigs.url[0], refspec: '+refs/heads/main:refs/remotes/origin/main']] ])
        script {
          docker.image('docker.rssw.eu/progress/dlc:12.8').inside('') {
            sh "ant -DDLC=/opt/progress/dlc -lib /opt/progress/dlc/pct/PCT.jar -lib xmltask.jar build"
          }
        }
      }
    }

    stage ('SonarQube') {
      agent { label 'Linux-Office03' }
      steps {
        script {
          docker.image('sonarsource/sonar-scanner-cli:latest').inside('') {
            withSonarQubeEnv('RSSW') {
              if ('main' == env.BRANCH_NAME) {
                sh "sonar-scanner -Dsonar.branch.name=$BRANCH_NAME"
              } else {
                sh "sonar-scanner -Dsonar.pullrequest.branch=$BRANCH_NAME -Dsonar.pullrequest.base=main -Dsonar.pullrequest.key=$BRANCH_NAME"
              }
            }
          }
        }
      }
    }

  }
}

