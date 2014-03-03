require 'rake'
require 'pathname'

DOTFILES_DIR = Pathname.new(Dir.home) + '.dotfiles'

namespace :install do
  task :homebrew do
    if !Kernel.system 'which brew > /dev/null'
      system 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'

      system 'brew tap phinze/cask'
      system 'brew install brew-cask'
    end
  end

  task :git => :homebrew do
    brew_install 'git'

    git_config_global('user.email', 'joakim.erelt@gmail.com')
    git_config_global('user.name', 'Joakim Erelt')
    git_config_global('alias.st', 'status')
    git_config_global('alias.co', 'checkout')
    git_config_global('alias.dog', 'log --decorate --oneline --graph --all')
    git_config_global('alias.punch', 'push --force')
    git_config_global('color.ui', 'true')
    git_config_global('core.autocrlf', 'input')
    git_config_global('push.default', 'simple')

    make_symlink 'gitignore'
    git_config_global('core.excludesfile', (home + '.gitignore'))
  end

  task :vim do
    make_symlink 'vim'
    make_symlink 'vimrc'
    mkdir_p home + 'Library/Vim'
  end

  task :dropbox do
    #brew_cask_install 'dropbox'
  end

  task :iterm => :homebrew do
    brew_cask_install 'iterm2'
  end

  task :sublime => :homebrew do
    brew_cask_install 'sublime-text'
  end

  task :pckh => :homebrew do
    brew_cask_install 'pckeyboardhack'
  end

  # TODO Move to :misc?
  task :casks => :homebrew do
    #brew_cask_install 'sourcetree'
    #brew_cask_install 'sizeup'
    #brew_cask_install 'virtualbox'
    #brew_cask_install 'kaleidoscope'
    #brew_cask_install 'cyberduck'
    #brew_cask_install 'the-unarchiver'
    brew_cask_install 'hex-fiend'
    #brew_cask_install 'airmail'
    #brew_cask_install 'skype'
    #brew_cask_install 'google-chrome'
  end

  task :dotfiles do
    Dir.chdir(Dir.home) { github_clone 'jocko/dotfiles', dotfiles_dir }
  end

  task :zsh => [:homebrew, :dotfiles] do
    brew_install 'zsh'
    Dir.chdir(Dir.home) { github_clone 'robbyrussell/oh-my-zsh', Pathname.new(Dir.home) + '.oh-my-zsh' }
    make_symlink 'zshrc'
    system 'chsh -s /bin/zsh' unless `echo $SHELL`.strip == '/bin/zsh'
    # TODO add to /etc/shells
  end

  task :rbenv => :homebrew do
    brew_install 'rbenv'
    brew_install 'rbenv-binstubs'
    brew_install 'ruby-build'

    rubies = ['2.1.1']
    rubies.each { |v| system 'rbenv', 'install', v unless ruby_installed?(v) }
    system 'rbenv', 'global', rubies.first
  end

  task :misc => :homebrew do
    mkdir_p home + 'Repos'

    brew_install 'mongodb'
    brew_install 'node'
  end

  task :all => [:rbenv, :zsh, :git, :misc] do end

  def dotfiles_dir
    Pathname.new(Dir.home) + '.dotfiles'
  end
end

namespace :backup do
  BACKUP_ROOT_DIR = Pathname.new(Dir.home) + 'Dropbox/Backups'

  task :check do
    abort "Location '#{BACKUP_ROOT_DIR}' does not exist or is not a directory" unless BACKUP_ROOT_DIR.directory?
  end

  task :init => :check do
    mkdir_p target_dir unless target_dir.exist?
  end

  task :sync => :init do
    rsync_a('~/Desktop')
    rsync_a('--delete',  '~/Downloads')
    rsync_a('~/Movies')
    rsync_a('~/Documents')
    rsync_a('~/Pictures/Photo\ Booth\ Library')

    system "rsync -aP ~/Repos #{target_dir}"
  end

  task :archive => :check do
    mv target_dir, target_dir.parent + "#{target_dir.basename}_#{Time.now.strftime('%Y-%m-%dT%H_%M_%S')}" if target_dir.exist?
  end

  def rsync_a(*args)
    system "rsync -aP --exclude=.DS_Store --exclude=.localized #{args.join(' ')} #{target_dir}"
  end

  def target_dir
    BACKUP_ROOT_DIR + platform_uuid + File.basename(Dir.home)
  end

  def platform_uuid
    `ioreg -rd1 -c IOPlatformExpertDevice | awk '/IOPlatformUUID/ { print $3; }' | tr -d \\"`.strip
  end
end

def github_clone(source, directory = nil)
  git_clone "git@github.com:#{source}.git", directory
end

def git_clone(repository, directory = nil)
  if directory
    path = Pathname.new(directory)
  else
    path = Pathname.new(File.basename(repository, '.git'))
  end
  system 'git', 'clone', repository, path.to_s unless path.exist?
end

def make_symlink(dotfile)
  target = home + ".#{dotfile}"
  ln_s dotfiles_dir + dotfile, target unless target.exist?
end

def home
  Pathname.new(Dir.home)
end

def sudo *args
  args = if args.length > 1
           args.unshift "/usr/bin/sudo"
         else
           "/usr/bin/sudo #{args.first}"
         end
  ohai *args
  system *args
end

def ohai(*args)
  puts args
end

def git_config_global(name, value)
  system 'git', 'config', '--global', name.to_s, value.to_s
end

def ruby_installed?(version)
   0 != system('rbenv versions --bare | grep ^2.1.1$ > /dev/null')
end

def system(*args)
  abort "Error executing: #{args}" unless Kernel.system(*args)
end

def formula_installed?(formula)
  Kernel.system("brew list #{formula} > /dev/null 2>&1")
end

def brew_install(formula)
  system "brew install #{formula}" unless formula_installed?(formula)
end

def brew_cask_install(cask)
  sh "brew cask install #{cask}"
end
