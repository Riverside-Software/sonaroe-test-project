pipeline {
  agent any

  stages {
    stage ('Build') {
      steps {
        script {
          echo "Create DB, execute Ant / PCT, generate ZIP file"
          withAnt(installation: 'Ant 1.10') {
            withEnv(["DLC=${tool name: 'OpenEdge-12.8', type: 'openedge'}"]) {
              echo "${BRANCH_NAME} --- ${BUILD_NUMBER}"
              if (isUnix()) {
                bat 'echo $BRANCH_NAME - $BUILD_NUMBER $JENKINS_HOME ${BRANCH_NAME}'
                sh 'ant -lib xmltask.jar -lib $DLC/pct/PCT.jar -DDLC=$DLC build'
              } else {
                bat 'echo %BRANCH_NAME% - %BUILD_NUMBER% %JENKINS_HOME% ${BRANCH_NAME}'
                bat 'ant -lib xmltask.jar -lib %DLC%\\pct\\PCT.jar -DDLC=%DLC% build'
              }
            }
          }
        }
      }
    }

    stage ('Code Analysis') {
      steps {
        echo "Execute SonarQube"
      }
    }

    stage ('Test') {
      steps {
        echo "Execute unit tests, regression tests, performance tests"
      }
    }

    stage ('Deploy') {
      steps {
        echo "Deployment to test environment"
      }
    }

  }
}

