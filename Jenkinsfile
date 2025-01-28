@Library('newcold') _

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
        script {
          def xx = checkoutCode('foo', 'bar')
          echo "Output: ${xx}"
        }
        script {
          dockerBuild('docker.rssw.eu/progress/dlc:12.8', 'build')
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

