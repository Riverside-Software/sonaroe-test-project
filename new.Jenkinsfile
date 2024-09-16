pipeline {
  agent any

  stages {
    stage ('Build') {
      steps {
        echo "Create DB, execute Ant / PCT, generate ZIP file on ${NODE_NAME}"
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

