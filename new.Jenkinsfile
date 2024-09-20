pipeline {
  agent any

  parameters {
    string(name: 'name', defaultValue: 'Jenkins',
          description: 'Target name')
    choice(name: 'environment', choices: 'UAT1,UAT2,QA1,QA2',
          description: 'Target environment')
  }

  stages {
    stage ('Build') {
      steps {
        script {
          echo "Create DB, execute Ant / PCT, generate ZIP file"

          bat "> git.txt git rev-parse --short HEAD"
          def commit = readFile('git.txt').trim()
          if (commit.startsWith("1")) {
            println "Commit ID starts with 1"
          } else {
            println "Boohooo..."
          }

          def suffix = env.BRANCH_NAME.contains("foobar") ? "XX" : "YY"
          long days = Long.parseLong(java.time.LocalDateTime.now().format(
            java.time.format.DateTimeFormatter.ofPattern('d')))
          long seconds = Long.parseLong(java.time.LocalDateTime.now().format(
            java.time.format.DateTimeFormatter.ofPattern('A'))) / 1000
          def artifactVersionNumber = java.time.LocalDateTime.now().format(
            java.time.format.DateTimeFormatter.ofPattern('YYYY.M.')) + ((days * 86400) + seconds) + suffix;
          println "Version number: ${artifactVersionNumber}"

          def ext = load("script.groovy")
          ext.prettyMessage(artifactVersionNumber)

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
      when {
        expression {
          BRANCH_NAME == 'main'
        }
      }
      steps {
        echo "Execute SonarQube"
      }
    }

    stage ('Test') {
      when {
        expression {
          BRANCH_NAME == 'Test'
        }
      }
      steps {
        echo "Execute unit tests, regression tests, performance tests"
      }
    }

    stage ('Deploy') {
      when {
        expression {
          BRANCH_NAME == 'Test'
        }
      }
      steps {
        echo "We'll deploy to environment ${environment} - With parameter ${name}"
      }
    }

  }
}

