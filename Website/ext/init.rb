# -*- encoding: utf-8 -*-
require 'tempfile'

module Webgen::Tag

  # provides syntax highlighting via the pygments command-line tool.
  class Pygments

    include Webgen::Tag::Base
    include Webgen::WebsiteAccess

    # highlight the body of the block.
    def call(tag, body, context)

      error_file = Tempfile.new('webgen-pygments')
      error_file.close

      `support/colorize.py 2>&1`
      if $?.exitstatus != 0
        raise Webgen::CommandNotFoundError.new('support/colorize.py', self.class.name, context.dest_node.alcn)
      end

      if param('tag.pygments.process_body')
        body = website.blackboard.invoke(:content_processor, 'tags').call(context.clone(:content => body)).content
      end
      cmd = "support/colorize.py #{param('tag.pygments.lang')} #{param('tag.pygments.tab_width')} 2>'#{error_file.path}'"
      result = IO.popen(cmd, 'r+') do |io|
        io.write(body)
        io.close_write
        io.read
      end
      if $?.exitstatus != 0
        File.foreach(error_file.path).each do |line|
          log(:warn) { "pygmentize> #{line}" }
        end
      end
		result
    end

  end

  class FilePygments

    include Webgen::Tag::Base
    include Webgen::WebsiteAccess

    # highlight the body of the block.
    def call(tag, body, context)

      error_file = Tempfile.new('webgen-pygmentsfile')
      error_file.close

      `support/colorize.py 2>&1`
      if $?.exitstatus != 0
        raise Webgen::CommandNotFoundError.new('support/colorize.py', self.class.name, context.dest_node.alcn)
      end

		body = File.read(param('tag.filepygments.filename'))
      cmd = "support/colorize.py #{param('tag.filepygments.lang')} #{param('tag.filepygments.tab_width')} 2>'#{error_file.path}'"
      result = IO.popen(cmd, 'r+') do |io|
        io.write(body)
        io.close_write
        io.read
      end
      if $?.exitstatus != 0
        File.foreach(error_file.path).each do |line|
          log(:warn) { "pygmentize> #{line}" }
        end
      end
		result
    end

  end

end

config = Webgen::WebsiteAccess.website.config
config['contentprocessor.tags.map']['pygments'] = 'Webgen::Tag::Pygments'
config['contentprocessor.tags.map']['pygmentsfile'] = 'Webgen::Tag::FilePygments'

config.tag.filepygments.lang('text', :doc => 'the language', :mandatory => 'default')
config.tag.filepygments.filename(nil, :doc => 'the source file', :mandatory => true)
config.tag.filepygments.tab_width(8, :doc => 'number of spaces used for a tabulator')

config.tag.pygments.lang('text', :doc => 'the language', :mandatory => 'default')
config.tag.pygments.tab_width(8, :doc => 'number of spaces used for a tabulator')
config.tag.pygments.process_body(false, :doc => 'the tag body will be scanned for tags first if true')
