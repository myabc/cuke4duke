module Cucumber
  class StepMother
    def load_programming_language(ext)
      puts 'ext## ' + ext
      lang = super.load_programming_language(ext)      
      @language_map['class'] = lang if ext == 'scala'
      puts lang.inspect
      lang      
    end
  end
end