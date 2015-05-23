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
  attr_reader :host, :domains, :sync

  def initialize(options)
    @host    = options['host']
    @box     = options['box']
    @size    = options['size']
    @domains = options.fetch('domains', nil) || []
    @domains = ["#{ @host }.dev"] + @domains
    @sync    = options.fetch('sync', nil) || []
    @sync    = @sync.map { |sync| process_sync(sync) }
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
    result = {}

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

  instances.each do |instance|
    vagrant.vm.define instance.host do |config|
      # Configure hostname
      #
      config.vm.hostname = instance.host

      # Configure box
      #
      config.vm.box              = instance.box
      config.vm.box_check_update = false

      # Configure ssh
      #
      config.ssh.username      = instance.username
      config.ssh.forward_agent = true

      # Configure network
      #
      config.vm.network(:private_network, type: 'dhcp')

      # Configure hostmanager
      #
      config.hostmanager.aliases = instance.domains

      # Configure sync folders
      #
      instance.sync.each do |sync|
        config.vm.synced_folder(sync.from, sync.to)
      end

      # Configure provider
      #
      config.vm.provider :parallels do |provider|
        provider.cpus               = instance.cpus
        provider.memory             = instance.memory
        provider.update_guest_tools = true
      end
    end
  end
end
