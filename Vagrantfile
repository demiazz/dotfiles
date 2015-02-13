require 'ostruct'
require 'pathname'
require 'yaml'

# Box
#
class Box
  attr_reader :name, :username, :key

  def initialize(options)
    @name     = options['name']
    @username = options['username']
    @key      = File.expand_path("~/.ssh/#{ options['key'] }")
  end
end

# Size
#
class Size
  attr_reader :cpus, :memory, :default

  def initialize(options)
    @cpus    = options.fetch('cpus', 1)
    @memory  = options.fetch('memory', 1024)
    @default = options.fetch('default', false)
  end
end

# Instance
#
class Instance
  attr_reader :host, :domain, :username, :groups, :sync, :cpus, :memory

  def initialize(options)
    @host     = options['host']
    @domain   = options.fetch('domain', "#{ @host }.dev")
    @username = options['username']
    @groups   = options.fetch('groups', [])
    @sync     = options.fetch('sync', []).map { |f| process_sync(f) }
    @cpus     = options['cpus']
    @memory   = options['memory']
  end

  private

  def process_sync(folder)
    result = { name: folder['name'] }

    result[:src] = if Pathname.new(folder['src']).absolute?
                     folder['src']
                   else
                     File.expand_path(File.join('~', folder['src']))
                   end

    result[:dest] = if Pathname.new(folder['dest']).absolute?
                      folder['dest']
                    else
                      File.expand_path(
                        File.join("/home/#{ @username }", folder['dest'])
                      )
                    end

    OpenStruct.new(result)
  end
end

# Settings
#
class Settings
  attr_reader :box, :instances

  def initialize
    load_settings

    create_box
    create_sizes
    create_instances
  end

  def size(name)
    @sizes[name]
  end

  private

  def load_settings
    @settings = YAML.load(File.read(File.expand_path('~/.vagrant.yml')))
  end

  def create_box
    @box = Box.new(@settings['box'])
  end

  def create_sizes
    @sizes = {}

    @settings['sizes'].each do |name, config|
      @sizes[name] = Size.new(config)
    end
  end

  def create_instances
    @instances = @settings['instances'].map do |instance|
      size = @sizes[instance['size']]

      Instance.new(
        instance.merge({
          'username' => @box.username,
          'cpus'     => size.cpus,
          'memory'   => size.memory
        })
      )
    end
  end
end

# IPManager
#
class IPManager
  attr_reader :registry

  def initialize
    @network  = "192.168.100"
    @node     = 10

    @registry = {}
  end

  def register_node(host, domain)
    ip = "#{ @network }.#{ @node }"

    @registry[host] = { 'domain' => domain, 'ip' => ip }

    @node += 1

    ip
  end
end

# Vagrant
#
Vagrant.configure(2) do |vagrant|
  settings   = Settings.new
  ip_manager = IPManager.new

  # Configure box
  #
  vagrant.vm.box              = settings.box.name
  vagrant.vm.box_check_update = false

  # Configure ssh
  #
  vagrant.ssh.username         = settings.box.username
  vagrant.ssh.private_key_path = settings.box.key
  vagrant.ssh.forward_agent    = true

  # Configure instances
  #
  settings.instances.each do |instance|
    ip = ip_manager.register_node(instance.host, instance.domain)

    vagrant.vm.define instance.host, autostart: true do |config|
      config.vm.hostname = instance.host

      config.vm.network(:private_network, ip: ip)

      instance.sync.each do |folder|
        config.vm.synced_folder(folder.src, folder.dest)
      end

      config.vm.provider :parallels do |provider|
        provider.cpus               = instance.cpus
        provider.memory             = instance.memory
        provider.update_guest_tools = true
      end
    end
  end

  # Provision
  #
  vagrant.vm.provision :ansible do |ansible|
    # Playbook
    #
    ansible.playbook = File.expand_path('~/.vagrant.d/ansible/playbook.yml')

    # Sudo
    #
    ansible.sudo = true

    # Ansible groups
    #
    ansible.groups = {}

    settings.instances.each do |instance|
      instance.groups.each do |group|
        ansible.groups[group] ||= []

        ansible.groups[group] << instance.host
      end
    end

    # Ansible vars
    #
    ansible.extra_vars = {}

    # Ansible user
    #
    ansible.extra_vars['user'] = settings.box.username

    # Ansible DNS table
    #
    ansible.extra_vars['ips'] = ip_manager.registry

    # Ansible synced folders
    #
    ansible.extra_vars['sync'] = {}

    settings.instances.each do |instance|
      folders = {}

      instance.sync.each do |folder|
        folders[folder.name] = folder.dest
      end

      ansible.extra_vars['sync'][instance.host] = folders
    end

    # Ansible command line arguments
    #
    ansible.raw_arguments = ['--ask-sudo-pass']
  end
end
