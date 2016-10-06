#!groovy

stage 'Build project'
node ('EC2-EU1B') {
  gitClean()
  checkout scm
  echo " Branch: ${env.BRANCH_NAME}"
  withEnv(["PATH+ANT=${tool name: 'Ant 1.9', type: 'hudson.tasks.Ant$AntInstallation'}/bin"]) {
    sh "ant -DDLC=Z:\\Progress\\OpenEdge-11.7 -lib Z:\\Tools\\PCT\\PCT-latest.jar build"
  }
  if ("master" == env.BRANCH_NAME) {
    sh "Z:\\Tools\\sonar-scanner\\bin\\sonar-scanner.bar -Dproject.settings=sonar-project1.properties"
  } else {
    withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'GitHub-GQuerret', usernameVariable: 'GH_LOGIN', passwordVariable: 'GH_PASSWORD']]) {
      sh "Z:\\Tools\\sonar-scanner\\bin\\sonar-scanner.bar -Dproject.settings=sonar-project2.properties -Dsonar.analysis.mode=issues -Dsonar.github.pullRequest=${env.BRANCH_NAME.substring(3)} -Dsonar.github.repository=Riverside-Software/sonaroe-test-project -Dsonar.github.oauth=${env.GH_PASSWORD}"
    }
  }
}

// see https://issues.jenkins-ci.org/browse/JENKINS-31924
def gitClean() {
    timeout(time: 60, unit: 'SECONDS') {
        if (fileExists('.git')) {
            echo 'Found Git repository: using Git to clean the tree.'
            // The sequence of reset --hard and clean -fdx first
            // in the root and then using submodule foreach
            // is based on how the Jenkins Git SCM clean before checkout
            // feature works.
            if (isUnix()) {
              sh 'git reset --hard'
            } else {
              bat 'git reset --hard'
            }
            // Note: -e is necessary to exclude the temp directory
            // .jenkins-XXXXX in the workspace where Pipeline puts the
            // batch file for the 'bat' command.
            if (isUnix()) {
              sh 'git clean -ffdx -e ".jenkins-*/"'
              sh 'git submodule foreach --recursive git reset --hard'
              sh 'git submodule foreach --recursive git clean -ffdx'
            } else {
              bat 'git clean -ffdx -e ".jenkins-*/"'
              bat 'git submodule foreach --recursive git reset --hard'
              bat 'git submodule foreach --recursive git clean -ffdx'
            }
        }
        else
        {
            echo 'No Git repository found: using deleteDir() to wipe clean'
            deleteDir()
        }
    }
}
