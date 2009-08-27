require 'cucumber/scala_support/backtrace_filter'
require 'forwardable'

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
        "YAY A SCALA SNIPPET: #{step_keyword}, #{step_name}, #{multiline_arg_class}"
      end
    end
  end
end

# class ::Java::Cuke4dukeInternalScala::ScalaLanguage
#   include ::Cucumber::LanguageSupport::LanguageMethods
# end
# 
# class ::Java::Cuke4dukeInternalScala::ScalaStepDefinition
#   include ::Cucumber::LanguageSupport::StepDefinitionMethods
# end
