begin
  class ::Java::Cuke4dukeInternalScala::ScalaStepDefinition
    include ::Cucumber::LanguageSupport::StepDefinitionMethods
  end
  class ::Java::Cuke4dukeInternalScala::ScalaAnalyzer
    def snippet_generator
      require 'cucumber/scala_support/scala_snippet_generator'
      Cucumber::JavaSupport::JavaSnippetGenerator.new
    end

    Cucumber::ClassSupport::ClassLanguage.analyzers << self.new
  end
rescue NameError
end