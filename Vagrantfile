# Instances
#

instances = [
  {
    host:   'ebay-social',
    domain: 'ebay.dev',
    groups: %w(rails),
    size:   :middle,
    sync:   [
      { name: 'application', from: '~/Code/ebay-social', to: 'ebay-social' }
    ]
  }
]

# Instance factory
#

class InstanceFactory
  SIZES    = {
    small:  { cpus: 1, memory: 1024 },
    middle: { cpus: 2, memory: 2048 },
    large:  { cpus: 4, memory: 4096 }
  }
  BOX      = 'ubuntu'
  USERNAME = 'demiazz'
  KEY      = 'vagrant'

  def initialize(instances)
    Vagrant.configure(2) do |config|
      configure_box(config)
      configure_ssh(config)

      instances.each do |i|
        configure_instance(config, i[:host], i[:domain], i[:size], i[:sync])
      end

      configure_provision(config, instances)
    end
  end

  def register_ip(host, domain)
    @ip_index ||= 10
    ip          = "192.168.100.#{ @ip_index }"
    @ip_index  += 1

    @ips ||= {}
    @ips[host] = {
      domain: domain || "#{ host }.dev",
      ip:     ip
    }

    ip
  end

  private

  def configure_instance(vagrant, host, domain, size, folders)
    ip = register_ip(host, domain)

    vagrant.vm.define host, autostart: false do |config|
      config.vm.hostname = host

      config.vm.network(:private_network, ip: ip)

      folders.each do |sf|
        from = sf[:from]
        to   = File.join("/home/#{ USERNAME }", sf[:to])

        config.vm.synced_folder(from, to)
      end

      config.vm.provider :parallels do |provider|
        provider.cpus               = SIZES[size][:cpus]
        provider.memory             = SIZES[size][:memory]
        provider.update_guest_tools = true
      end
    end
  end

  def configure_box(vagrant)
    vagrant.vm.box              = BOX
    vagrant.vm.box_check_update = false
  end

  def configure_ssh(vagrant)
    vagrant.ssh.username         = USERNAME
    vagrant.ssh.private_key_path = File.join('~/.ssh', KEY)
    vagrant.ssh.forward_agent    = true
  end

  def configure_provision(vagrant, instances)
    registered_ips = @ips

    vagrant.vm.provision :ansible do |ansible|
      # Ansible playbook
      #
      ansible.playbook = File.expand_path('~/.vagrant.d/ansible/playbook.yml')

      # Sudo
      #
      ansible.sudo = true

      # Ansible groups
      #
      ansible.groups = {}
      instances.each do |instance|
        instance_groups = instance.fetch(:groups, [])

        instance_groups.each do |group|
          ansible.groups[group] ||= []

          ansible.groups[group].push(instance[:host])
        end
      end

      # User for working in VM
      #
      vars = { user: USERNAME, sync: {} }

      instances.each do |instance|
        folders = {}

        instance[:sync].each do |folder|
          name = folder[:name]
          to   = File.join("/home/#{ USERNAME }", folder[:to])

          folders[name] = to
        end

        vars[:sync][instance[:host]] = folders
      end

      # Domains and ips for registration
      #
      vars[:ips] = registered_ips

      # Transit vars to ansible
      #
      ansible.extra_vars = vars

      # Ask password for sudo
      #
      ansible.raw_arguments = ['--ask-sudo-pass']
    end

    def expand_sync_path(path)
      File.join("/home/#{ USERNAME }", path)
    end
  end
end

# Create instances
#

InstanceFactory.new(instances)
