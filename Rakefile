# TODO
#
# gem install lunchy
# rubymine settings: camelhumps, confirm exit, code style, line numbering, ^R should map to rename
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
require 'erb'

module ITerm
  def self.install_theme(name, colors)
    template = <<END
defaults write
-app iTerm
'Custom Color Presets'
-dict-add '<%= name %>'
'{
  "Ansi 0 Color" = {
    "Red Component" = "<%= colors[:ansi_0_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_0_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_0_color][:blue] %>";
  };
  "Ansi 1 Color" = {
    "Red Component" = "<%= colors[:ansi_1_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_1_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_1_color][:blue] %>";
  };
  "Ansi 2 Color" = {
    "Red Component" = "<%= colors[:ansi_2_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_2_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_2_color][:blue] %>";
  };
  "Ansi 3 Color" = {
    "Red Component" = "<%= colors[:ansi_3_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_3_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_3_color][:blue] %>";
  };
  "Ansi 4 Color" = {
    "Red Component" = "<%= colors[:ansi_4_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_4_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_4_color][:blue] %>";
  };
  "Ansi 5 Color" = {
    "Red Component" = "<%= colors[:ansi_5_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_5_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_5_color][:blue] %>";
  };
  "Ansi 6 Color" = {
    "Red Component" = "<%= colors[:ansi_6_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_6_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_6_color][:blue] %>";
  };
  "Ansi 7 Color" = {
    "Red Component" = "<%= colors[:ansi_7_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_7_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_7_color][:blue] %>";
  };
  "Ansi 8 Color" = {
    "Red Component" = "<%= colors[:ansi_8_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_8_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_8_color][:blue] %>";
  };
  "Ansi 9 Color" = {
    "Red Component" = "<%= colors[:ansi_9_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_9_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_9_color][:blue] %>";
  };
  "Ansi 10 Color" = {
    "Red Component" = "<%= colors[:ansi_10_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_10_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_10_color][:blue] %>";
  };
  "Ansi 11 Color" = {
    "Red Component" = "<%= colors[:ansi_11_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_11_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_11_color][:blue] %>";
  };
  "Ansi 12 Color" = {
    "Red Component" = "<%= colors[:ansi_12_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_12_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_12_color][:blue] %>";
  };
  "Ansi 13 Color" = {
    "Red Component" = "<%= colors[:ansi_13_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_13_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_13_color][:blue] %>";
  };
  "Ansi 14 Color" = {
    "Red Component" = "<%= colors[:ansi_14_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_14_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_14_color][:blue] %>";
  };
  "Ansi 15 Color" = {
    "Red Component" = "<%= colors[:ansi_15_color][:red] %>";
    "Green Component" = "<%= colors[:ansi_15_color][:green] %>";
    "Blue Component" = "<%= colors[:ansi_15_color][:blue] %>";
  };
  "Background Color" = {
    "Red Component" = "<%= colors[:background_color][:red] %>";
    "Green Component" = "<%= colors[:background_color][:green] %>";
    "Blue Component" = "<%= colors[:background_color][:blue] %>";
  };
  "Bold Color" = {
    "Red Component" = "<%= colors[:bold_color][:red] %>";
    "Green Component" = "<%= colors[:bold_color][:green] %>";
    "Blue Component" = "<%= colors[:bold_color][:blue] %>";
  };
  "Cursor Color" = {
    "Red Component" = "<%= colors[:cursor_color][:red] %>";
    "Green Component" = "<%= colors[:cursor_color][:green] %>";
    "Blue Component" = "<%= colors[:cursor_color][:blue] %>";
  };
  "Cursor Text Color" = {
    "Red Component" = "<%= colors[:cursor_text_color][:red] %>";
    "Green Component" = "<%= colors[:cursor_text_color][:green] %>";
    "Blue Component" = "<%= colors[:cursor_text_color][:blue] %>";
  };
  "Foreground Color" = {
    "Red Component" = "<%= colors[:foreground_color][:red] %>";
    "Green Component" = "<%= colors[:foreground_color][:green] %>";
    "Blue Component" = "<%= colors[:foreground_color][:blue] %>";
  };
  "Selected Text Color" = {
    "Red Component" = "<%= colors[:selected_text_color][:red] %>";
    "Green Component" = "<%= colors[:selected_text_color][:green] %>";
    "Blue Component" = "<%= colors[:selected_text_color][:blue] %>";
  };
  "Selection Color" = {
    "Red Component" = "<%= colors[:selection_color][:red] %>";
    "Green Component" = "<%= colors[:selection_color][:green] %>";
    "Blue Component" = "<%= colors[:selection_color][:blue] %>";
  };
}'
END

    `#{ERB.new(template).result(OpenStruct.new({name: name, colors: colors}).instance_eval { binding }).gsub("\n", ' ')}`
  end
end

task :twilight do
  name = 'Twilight'
  colors = {
      ansi_0_color: {red: '0.078830234706401825', green: '0.07883714884519577', blue: '0.078821629285812378'},
      ansi_1_color: {red: '0.7543104887008667', green: '0.42739877104759216', blue: '0.26590591669082642'},
      ansi_10_color: {red: '0.79963678121566772', green: '0.84882640838623047', blue: '0.55018079280853271'},
      ansi_11_color: {red: '0.88534402847290039', green: '0.76867014169692993', blue: '0.49316486716270447'},
      ansi_12_color: {red: '0.35298055410385132', green: '0.36815744638442993', blue: '0.38419744372367859'},
      ansi_13_color: {red: '0.81614553928375244', green: '0.86099404096603394', blue: '0.55795210599899292'},
      ansi_14_color: {red: '0.54136258363723755', green: '0.59707218408584595', blue: '0.60617965459823608'},
      ansi_15_color: {red: '0.99986904859542847', green: '1', blue: '0.83107829093933105'},
      ansi_2_color: {red: '0.68779230117797852', green: '0.72692984342575073', blue: '0.47654882073402405'},
      ansi_3_color: {red: '0.76260745525360107', green: '0.66052716970443726', blue: '0.42418986558914185'},
      ansi_4_color: {red: '0.26757293939590454', green: '0.27791988849639893', blue: '0.28883245587348938'},
      ansi_5_color: {red: '0.70590567588806152', green: '0.74553430080413818', blue: '0.4848540723323822'},
      ansi_6_color: {red: '0.46818926930427551', green: '0.51336991786956787', blue: '0.52099674940109253'},
      ansi_7_color: {red: '0.99986904859542847', green: '1', blue: '0.83107829093933105'},
      ansi_8_color: {red: '0.15055373311042786', green: '0.15056693553924561', blue: '0.15053729712963104'},
      ansi_9_color: {red: '0.86878842115402222', green: '0.48694252967834473', blue: '0.29907804727554321'},
      background_color: {red: '0.078830234706401825', green: '0.07883714884519577', blue: '0.078821629285812378'},
      bold_color: {red: '0.99986904859542847', green: '1', blue: '0.83107829093933105'},
      cursor_color: {red: '1', green: '1', blue: '1'},
      cursor_text_color: {red: '0.0', green: '0.0', blue: '0.0'},
      foreground_color: {red: '0.99986904859542847', green: '1', blue: '0.83107829093933105'},
      selected_text_color: {red: '0.99986904859542847', green: '1', blue: '0.83107829093933105'},
      selection_color: {red: '0.19196827709674835', green: '0.1919851154088974', blue: '0.1919473260641098'},
  }

  ITerm.install_theme(name, colors)
end

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

  cask_install 'sublime-text' do
    sublime_user_dir = home('Library/Application Support/Sublime Text 2/Packages/User')
    rm_r sublime_user_dir if FileTest.directory?(sublime_user_dir)
    ln_s dotfiles_dir.join('sublimetext2'), sublime_user_dir
    # TODO Tomorrow Night Theme
    # TODO Hide Minimap
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
    home('.dotfiles')
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

def home(target = nil)
  Pathname.new(Dir.home).join(*[target])
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

def system(*args)
  abort "Error executing: #{args}" unless Kernel.system(*args)
end

# TODO Remove
def cask_installed?(cask)
  Kernel.system("brew cask list #{cask} > /dev/null 2>&1")
end

# TODO Remove
def brew_cask_install(cask)
  sh "brew cask install #{cask}" unless cask_installed?(cask)
end
