require 'ostruct'
require 'pathname'
require 'yaml'


VAGRANT_ROOT = File.expand_path('~/.vagrant.d')


class Box
  attr_reader :name, :username

  def initialize(name, username)
    @name     = name
    @username = username
  end
end


class Size
  attr_reader :cpus, :memory

  def initialize(cpus, memory)
    @cpus   = cpus
    @memory = memory
  end
end


class Instance
  attr_reader :host, :domains, :sync, :groups

  def initialize(options)
    @host    = options['host']
    @box     = options['box']
    @size    = options['size']
    @domains = options.fetch('domains', nil) || []
    @domains = ["#{ @host }.dev"] + @domains
    @sync    = options.fetch('sync', nil) || []
    @sync    = @sync.map { |sync| process_sync(sync) }
    @groups  = options.fetch('groups', nil) || []
  end

  def box
    @box.name
  end

  def username
    @box.username
  end

  def cpus
    @size.cpus
  end

  def memory
    @size.memory
  end

  private

  def process_sync(sync)
    result = { name: sync['name'] }

    result['from'] = if Pathname.new(sync['from']).absolute?
                       sync['from']
                     else
                       File.expand_path(File.join('~', sync['from']))
                     end

    result['to'] = if Pathname.new(sync['to']).absolute?
                     sync['to']
                   else
                     File.join("/home/#{ username }", sync['to'])
                   end

    OpenStruct.new(result)
  end
end


class Presets
  PRESETS_FILE = File.join(VAGRANT_ROOT, 'presets.yml')

  def initialize
    load
    parse
  end

  def box(name)
    @boxes[name]
  end

  def size(name)
    @sizes[name]
  end

  private

  def load
    @presets = YAML.load(File.read(PRESETS_FILE))
  end

  def parse
    parse_boxes
    parse_sizes
  end

  def parse_boxes
    @boxes = {}

    @presets['boxes'].each do |box_name, box_attrs|
      @boxes[box_name] = Box.new(box_attrs['name'], box_attrs['username'])
    end
  end

  def parse_sizes
    @sizes = {}

    @presets['sizes'].each do |size_name, size_attrs|
      @sizes[size_name] = Size.new(size_attrs['cpus'], size_attrs['memory'])
    end
  end
end


class InstanceFactory
  PUBLIC_INSTANCES  = File.join(VAGRANT_ROOT, 'instances.public.yml')
  PRIVATE_INSTANCES = File.join(VAGRANT_ROOT, 'instances.private.yml')

  def initialize(presets)
    @presets = presets

    load
    parse
  end

  def each(&block)
    @instances.each(&block)
  end

  private

  def load
    public_instances  = YAML.load(File.read(PUBLIC_INSTANCES))
    private_instances = if File.exist?(PRIVATE_INSTANCES)
                          YAML.load(File.read(PRIVATE_INSTANCES))
                        end

    public_instances  ||= []
    private_instances ||= []

    @raw_instances = public_instances + private_instances
  end

  def parse
    @instances = @raw_instances.map do |instance|
      parse_instance(instance)
    end
  end

  def parse_instance(instance)
    options = {}.merge(instance)

    options['box']  = @presets.box(options['box'])
    options['size'] = @presets.size(options['size'])

    Instance.new(options)
  end
end


Vagrant.configure(2) do |vagrant|
  presets   = Presets.new
  instances = InstanceFactory.new(presets)

  vagrant.hostmanager.enabled           = true
  vagrant.hostmanager.manage_host       = true
  vagrant.hostmanager.ignore_private_ip = false
  vagrant.hostmanager.include_offline   = true

  vagrant.vm.provision :hostmanager

  instances.each do |config|
    vagrant.vm.define config.host do |node|
      # Configure hostname
      #
      node.vm.hostname = config.host

      # Configure box
      #
      node.vm.box              = config.box
      node.vm.box_check_update = false

      # Configure ssh
      #
      node.ssh.username      = config.username
      node.ssh.forward_agent = true

      # Configure network
      #
      node.vm.network(:private_network, type: 'dhcp')

      # Configure hostmanager
      #
      node.hostmanager.aliases = config.domains

      # Configure sync folders
      #
      config.sync.each do |sync|
        node.vm.synced_folder(sync.from, sync.to)
      end

      # Configure provider
      #
      node.vm.provider :parallels do |provider|
        provider.cpus               = config.cpus
        provider.memory             = config.memory
        provider.update_guest_tools = true

        provider.customize [ 'set', :id,
                             '--on-window-close', 'keep-running' ]
      end
    end
  end

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

    instances.each do |instance|
      instance.groups.each do |group|
        ansible.groups[group] ||= []

        ansible.groups[group] << instance.host
      end
    end

    # Ansible vars
    #
    ansible.extra_vars = {}

    # Ansible synced folders
    #
    ansible.extra_vars['sync'] = {}

    instances.each do |config|
      folders = {}

      config.sync.each do |sync|
        folders[sync.name] = sync.to
      end

      ansible.extra_vars['sync'][config.host] = folders
    end

    # Ansible domains
    #
    ansible.extra_vars['domains'] = {}

    instances.each do |config|
      ansible.extra_vars['domains'][config.host] = config.domains
    end
  end
end
