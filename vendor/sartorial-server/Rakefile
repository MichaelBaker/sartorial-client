desc "Setup the project for the first time"
task :setup do
  sh "cabal sandbox add-source vendor/sartorial"
  sh "cabal sandbox add-source vendor/sartorial-client"
end

namespace :update do
  def pull_vendor_subtree(project_name, git_url)
    sh "git subtree pull --prefix vendor/#{project_name} #{git_url} master --squash"
  end

  desc "Update all dependencies in 'vendor'"
  task :all => [:sartorial, :sartorial_client]

  desc "Update the sartorial repository"
  task :sartorial do
    pull_vendor_subtree("sartorial", "git@github.com:MichaelBaker/sartorial.git")
  end

  desc "Update the sartorial-client repository"
  task :sartorial_client do
    pull_vendor_subtree("sartorial-client", "git@github.com:MichaelBaker/sartorial-client.git")
  end
end
