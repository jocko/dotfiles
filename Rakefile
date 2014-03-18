# TODO
#
# gem install lunchy
# rubymine settings: camelhumps, confirm exit, code style, line numbering, ^R should map to rename
# gem install bundler
# sizeup
## Start SizeUp at login
#defaults write com.irradiatedsoftware.SizeUp StartAtLogin -bool true
#
## Don’t show the preferences window on next start
#defaults write com.irradiatedsoftware.SizeUp ShowPrefsOnNextStart -bool false
#
# configuration like wrapper around plist domains, finder, safari etc?
# wallpapers
require 'rake'
require 'pathname'

module Finder
  def self.configure
    (@configuration ||= Configuration.new).tap { |cfg| yield(cfg) }
  end

  class Configuration
    attr_accessor :show_status_bar, :show_path_bar, :extension_change_warning, :show_all_files, :show_all_extensions, :icon_size
    # Possible values: :current_folder, :this_mac, :use_previous
    attr_accessor :default_search_scope
    # Possible values: :icon, :list, :column, :coverflow
    attr_accessor :preferred_view_style
    # Possible values: :computer, :volume, :home, :desktop, :documents, :all_files
    attr_accessor :new_window_target

    def initialize
      @show_status_bar = false
      @show_path_bar = false
      @default_search_scope = :this_mac
      @extension_change_warning = true
      @preferred_view_style = :icon
      @show_all_files = false
      @show_all_extensions = false
      @new_window_target = :all_files
      @icon_size = 64
    end

    def persist
      defaults('com.apple.finder').tap do |d|
        d.write 'ShowStatusBar', show_status_bar
        d.write 'FXDefaultSearchScope', dss_to_s
        d.write 'FXEnableExtensionChangeWarning', extension_change_warning
        d.write 'FXPreferredViewStyle', pvs_to_s
        d.write 'NewWindowTarget', nwt_to_s
        d.write 'ShowPathbar', show_path_bar
      end

      defaults_global.tap do |d|
        d.write 'AppleShowAllExtensions', show_all_extensions
      end

      system "/usr/libexec/PlistBuddy -c 'Set :DesktopViewSettings:IconViewSettings:iconSize #{icon_size}' ~/Library/Preferences/com.apple.finder.plist"
    end

    def nwt_to_s
      {computer: 'PfCm', volume: 'PfVo', home: 'PfHm', desktop: 'PfDe', documents: 'PfDo', all_files: 'PfAF'}.fetch(@new_window_target)
    end

    def dss_to_s
      {current_folder: 'SCcf', this_mac: 'SCev', use_previous: 'SCsp'}.fetch(@default_search_scope)
    end

    def pvs_to_s
      {icon: 'icnv', list: 'Nlsv', column: 'clmv', coverflow: 'Flwv'}.fetch(@preferred_view_style)
    end
  end
end

def configure(*args, &block)
  Rake::Task.define_task(*args) do
    domain = Rake.application.resolve_args(args).first
    Kernel.const_get(domain.capitalize).configure { |cfg| block.call(cfg) }.persist
  end
end

class SymbolicHotKey
  attr_accessor :key, :enabled, :parameters

  def initialize(key, parameters, enabled)
    @key = key
    @parameters = parameters
    @enabled = enabled
  end

  def write
    system "/usr/libexec/PlistBuddy -c 'Delete :AppleSymbolicHotKeys:#{key}' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
    system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:enabled bool #{enabled}' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
    if enabled
      system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:value:type string standard' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
      system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:value:parameters array' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
      system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:value:parameters:0 integer #{parameters[0]}' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
      system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:value:parameters:1 integer #{parameters[1]}' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
      system "/usr/libexec/PlistBuddy -c 'Add :AppleSymbolicHotKeys:#{key}:value:parameters:2 integer #{parameters[2]}' ~/Library/Preferences/com.apple.symbolichotkeys.plist"
    end
  end

  def self.enable(key, parameters)
    SymbolicHotKey.new(key, parameters, true).tap(&:write)
  end

  def self.disable(key)
    SymbolicHotKey.new(key, [], false).tap(&:write)
  end
end

module Homebrew
  class << self
    def need_install?(formula)
      !installed?(formula)
    end

    def installed?(formula)
      Kernel.system("brew list #{formula} > /dev/null 2>&1")
    end

    def install(formula)
      system "brew install #{formula}" unless installed?(formula)
      yield if block_given?
    end

    def install_homebrew
      if !Kernel.system 'which brew > /dev/null'
        system 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'

        # TODO Ideally, yield to task and perform this there
        system 'brew tap phinze/cask'
        system 'brew install brew-cask'
      end
    end
  end
end

module Cask
  class << self
    def installed?(cask)
      Kernel.system("brew cask list #{cask} > /dev/null 2>&1")
    end

    def install(cask)
      sh "brew cask install #{cask}" unless cask_installed?(cask)
    end
  end
end

task :install_homebrew do
  Homebrew.install_homebrew
end

# Install a Homebrew formula.
#
# Example: brew_install :wget
def brew_install(*args, &block)
  Rake::Task.define_task(*args) do
    formula = Rake.application.resolve_args(args).first
    Homebrew.install(formula) { block.call if block } unless Homebrew.installed?(formula)
  end.enhance([:install_homebrew])
end

def cask_install(*args, &block)
  Rake::Task.define_task(*args) do
    cask = Rake.application.resolve_args(args).first
    Cask.install(cask) { block.call if block } unless Cask.installed?(cask)
  end.enhance([:install_homebrew])
end

#brew_install :rbenv
brew_install 'ruby-build'
brew_install 'rbenv-binstubs'

def rbenv_install(name, version, use = false)
  Rake::Task.define_task(name) do
    system 'rbenv', 'install', version unless ruby_installed?(version)
    system 'rbenv', 'global', version if use
  end.enhance(%w(ruby-build rbenv-binstubs)) #
end

#task :rbenv => :homebrew do
#  brew_install 'rbenv'
#  brew_install 'rbenv-binstubs'
#  brew_install 'ruby-build'
#
#  rubies = ['2.1.1']
#  rubies.each { |v| system 'rbenv', 'install', v unless ruby_installed?(v) }
#  system 'rbenv', 'global', rubies.first
#end

class Defaults
  def initialize(domain)
    @domain = domain
  end

  def write(key, value)
    #puts "defaults write #{@domain} #{key} #{Defaults.to_value(value)}"
    `defaults write #{@domain} #{key} #{to_value(value)}`
  end

  def to_value(o)
    case o
      when Fixnum
        "-int #{o}"
      when TrueClass
        '-bool true'
      when FalseClass
        '-bool false'
      when Hash
        [].tap do |d|
          d << '-dict'
          o.each_pair do |k, v|
            d << k
            d << to_value(v)
          end
        end.join(' ')
      when String
        "-string '#{o}'"
      when Float
        "-float #{o}"
      else
        fail "Unsupported type: #{o.class}"
    end
  end
end

def defaults(domain, &block)
  if block_given?
    Defaults.new(domain).instance_exec(&block)
  else
    Defaults.new(domain)
  end
end

def defaults_global(&block)
  defaults 'NSGlobalDomain', &block
end

DOTFILES_DIR = Pathname.new(Dir.home) + '.dotfiles'

namespace :install do
  #task :homebrew do
  #  if !Kernel.system 'which brew > /dev/null'
  #    system 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'
  #
  #    system 'brew tap phinze/cask'
  #    system 'brew install brew-cask'
  #  end
  #end
  rbenv_install :ruby_2_1, '2.1.1', true
  rbenv_install :ruby_2_0, '2.0.0-p451'

  brew_install :git do
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

  task :iterm => :homebrew do
    brew_cask_install 'iterm2'

    # Install pretty iTerm colors
    #open "${HOME}/init/Mathias.itermcolors"

    # Don’t display the annoying prompt when quitting iTerm
#    defaults write com.googlecode.iterm2 PromptOnQuit -bool false
  end

  task :sublime => :homebrew do
    brew_cask_install 'sublime-text'
    #http://zanshin.net/2013/01/21/sublime-text-2-dotfiles-simplified/
  end

  task :pckh => :homebrew do
    brew_cask_install 'pckeyboardhack'

    # Disable the caps lock key
    #
    # Caveat:
    # The last part of the modifiermapping key below consist of the vendor id (1452 is Apple)
    # and product id of the keyboard (595 is 'Apple Internal Keyboard / Trackpad' and 592 is
    # 'Apple Keyboard'). These values can be obtained via `ioreg -n IOHIDKeyboard -r'`.
    `defaults -currentHost write -g com.apple.keyboard.modifiermapping.1452-595-0 -array '<dict><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer><key>HIDKeyboardModifierMappingDst</key><integer>-1</integer></dict>'`
    `defaults -currentHost write -g com.apple.keyboard.modifiermapping.1452-592-0 -array '<dict><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer><key>HIDKeyboardModifierMappingDst</key><integer>-1</integer></dict>'`

    # Map caps lock to escape
    defaults 'org.pqrs.PCKeyboardHack' do
       write 'sysctl', { 'enable_capslock' => true, 'keycode_capslock' => 53 }
    end
  end

  cask_install :rubymine do
    # TODO Would be nice to configure RubyMine as well!
  end

  brew_install :zsh do
    Dir.chdir(Dir.home) { github_clone 'robbyrussell/oh-my-zsh', Pathname.new(Dir.home) + '.oh-my-zsh' }
    make_symlink 'zshrc'
    system 'chsh -s /bin/zsh' unless `echo $SHELL`.strip == '/bin/zsh'
    # TODO add to /etc/shells
  end

  #task :rbenv => :homebrew do
  #  brew_install 'rbenv'
  #  brew_install 'rbenv-binstubs'
  #  brew_install 'ruby-build'
  #
  #  rubies = ['2.1.1']
  #  rubies.each { |v| system 'rbenv', 'install', v unless ruby_installed?(v) }
  #  system 'rbenv', 'global', rubies.first
  #end

  task :misc => :install_homebrew do
    mkdir_p home + 'Repos'

    #brew_install 'mongodb'
    #brew_install 'node'
    brew_install :wget
    brew_install :the_silver_searcher

    brew_cask_install 'spotify'
    brew_cask_install 'sourcetree'
    brew_cask_install 'sizeup'
    #brew_cask_install 'virtualbox'
    #brew_cask_install 'kaleidoscope'
    brew_cask_install 'cyberduck'
    brew_cask_install 'the-unarchiver'
    brew_cask_install 'hex-fiend'
    #brew_cask_install 'airmail'
    #brew_cask_install 'skype'
    brew_cask_install 'google-chrome'

    defaults 'com.apple.dashboard' do
      # Disable Dashboard
      write 'mcx-disabled', true
    end

    defaults_global do
      write 'InitialKeyRepeat', 14
      write 'KeyRepeat', 2

      write 'com.apple.sound.beep.feedback', 0
    end

    defaults 'NSGlobalDomain' do
      # Disable Resume system-wide
      write 'NSQuitAlwaysKeepsWindows', false
    end

    defaults 'com.apple.menuextra.clock' do
      # Use a 24-hour clock
      write 'DateFormat', 'EEE HH:mm'
    end

    # Disable Notification Center and remove the menu bar icon
    system 'launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null'


    defaults 'com.apple.systemsound' do
      write 'com.apple.sound.beep.volume', 0.0
    end
  end

  task :dock do
    defaults 'com.apple.dock' do
      # Hot corners
      # Possible values:
      #  0: no-op
      #  2: Mission Control
      #  3: Show application windows
      #  4: Desktop
      #  5: Start screen saver
      #  6: Disable screen saver
      #  7: Dashboard
      # 10: Put display to sleep
      # 11: Launchpad
      # 12: Notification Center

      # Top left screen corner => Start screen saver
      write 'wvous-tl-corner', 5
      write 'wvous-tl-modifier', 0

      # Bottom right screen corner => Desktop
      write 'wvous-br-corner', 4
      write 'wvous-br-modifier', 0

      # Bottom left screen corner => Mission Control
      write 'wvous-bl-corner', 2
      write 'wvous-bl-modifier', 0

      # Automatically hide and show the Dock
      write 'autohide', true
    end
  end

  task :sound do
    defaults 'com.apple.systemsound' do
      write 'com.apple.sound.beep.volume', 0.0
    end

    defaults_global do
      write 'com.apple.sound.beep.feedback', 0
    end
  end

  task :safari do
    defaults 'com.apple.Safari' do
      # New tabs open with empty page
      write 'NewTabBehavior', 1

      # Set Safari’s home page
      write 'HomePage', 'www.google.com'

      # Prevent Safari from opening ‘safe’ files automatically after downloading
      write 'AutoOpenSafeDownloads', false

      # Enable Safari’s debug menu
      write 'IncludeInternalDebugMenu', true

      # Enable the Develop menu and the Web Inspector in Safari
      write 'IncludeDevelopMenu', true
      write 'WebKitDeveloperExtrasEnabledPreferenceKey', true
      write 'com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled', true

      # Show status bar
      write 'ShowStatusBar', true

      write 'NSQuitAlwaysKeepsWindows', true
    end
  end

  task :trackpad do
    defaults_global do
      # TODO Check if I actually need this
      #write 'com.apple.mouse.tapBehavior', 1, current_host: true
    end

    # Trackpad
    defaults 'com.apple.driver.AppleBluetoothMultitouch.trackpad' do
      # Enable tap to click
      write 'Clicking', true
      # Enable three finger drag
      write 'TrackpadThreeFingerDrag', true
    end
  end

  task :keyboard do
    defaults_global do
      # Use all F1, F2, etc. keys as standard function keys
      write 'com.apple.keyboard.fnState', true
      # Disable press-and-hold for keys in favor of key repeat
      write 'ApplePressAndHoldEnabled', false
    end
  end

  configure :finder do |prefs|
    prefs.show_status_bar = true
    prefs.show_path_bar = true
    prefs.default_search_scope = :current_folder
    prefs.extension_change_warning = false
    prefs.preferred_view_style = :column
    prefs.new_window_target = :home
    prefs.icon_size = 44
  end

  task :symbolichotkeys do
    # Mission Control: Mission Control => ^↑
    SymbolicHotKey.disable(32)
    SymbolicHotKey.disable(34)
    # Mission Control: Application Windows => ^↓
    SymbolicHotKey.disable(33)
    SymbolicHotKey.disable(35)
    # Mission Control: Move left a space => ^←
    SymbolicHotKey.disable(79)
    # Mission Control: Move right a space => ^→
    SymbolicHotKey.disable(81)

    # Accessibility: Turn VoiceOver on or off => ⌘F5
    SymbolicHotKey.disable(59)
    # Accessibility: Show Accessibility controls => ⌥⌘F5
    SymbolicHotKey.disable(162)

    # App Shortcuts: Show Help menu => ⇧⌘/
    SymbolicHotKey.disable(98)

    # Keyboard: Move focus to next window => ⌘§
    SymbolicHotKey.enable(27, [167, 10, 1048576])
  end

  # TODO sudo -v
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
   Kernel.system("rbenv versions --bare | grep ^#{version}$ > /dev/null")
end

def rbenv_rehash
  system 'rbenv', 'rehash'
end

def system(*args)
  abort "Error executing: #{args}" unless Kernel.system(*args)
end

#def formula_installed?(formula)
#  Kernel.system("brew list #{formula} > /dev/null 2>&1")
#end
#
#def brew_install(formula)
#  system "brew install #{formula}" unless formula_installed?(formula)
#end

def cask_installed?(cask)
  Kernel.system("brew cask list #{cask} > /dev/null 2>&1")
end

def brew_cask_install(cask)
  sh "brew cask install #{cask}" unless cask_installed?(cask)
end
