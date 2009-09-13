require 'cucumber/scala_support/backtrace_filter'
require 'forwardable'
require 'iconv'

module Cucumber
  module ScalaSupport
    class ScalaLanguage
      extend Forwardable
      include ::Cucumber::LanguageSupport::LanguageMethods

      def_delegators :@delegate, :step_definitions_for, :begin_scenario, :end_scenario

      def initialize(step_mother)
        @delegate = ::Java::Cuke4dukeInternalScala::ScalaLanguage.new(self)
      end

      def alias_adverbs(adverbs)
      end

      def snippet_text(step_keyword, step_name, multiline_arg_class = nil)
        escaped = Regexp.escape(step_name).gsub('\ ', ' ').gsub('/', '\/')
        escaped = escaped.gsub(PARAM_PATTERN, ESCAPED_PARAM_PATTERN)

        n = 0
        args = escaped.scan(ESCAPED_PARAM_PATTERN).map do |a|
          n += 1
          "arg#{n}:String"
        end
        args << "#{multiline_arg_class.default_arg_name}:TODO.Table" unless multiline_arg_class.nil?
        arg_string = args.join(", ")
        
        %(#{step_keyword}("^#{escaped}$") { #{arg_string} =>\n  pending\n}\n)                
      end

      private

      PARAM_PATTERN = /"([^\"]*)"/
      ESCAPED_PARAM_PATTERN = '\\"([^\\"]*)\\"'

    end
  end
end

class ::Java::Cuke4dukeInternalScala::ScalaStepDefinition
  include ::Cucumber::LanguageSupport::StepDefinitionMethods
end