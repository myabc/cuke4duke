desc 'Make all files use UNIX (\n) line endings'
task :fix_cr_lf do
  files = FileList['**/*']
  files.each do |f|
    next if File.directory?(f)
    s = IO.read(f)
    s.gsub!(/\r?\n/, "\n")
    File.open(f, "w") { |io| io.write(s) }
  end
end

desc 'Release'
task :release do
  version = IO.read('pom.xml').match(/<version>(.*)<\/version>/)[1]
  sh %{mvn clean -P examples install}
  sh %{mvn deploy}
  Dir.chdir('cuke4duke') do
    sh %{mvn site:site site:deploy}
  end
  sh %{git commit -a -m "Release #{version}"}
  sh %{git tag -a "v#{version}" -m "Release #{version}"}
  sh %{git push}
  sh %{git push --tags}
end

desc "Update Scala's I18N Language traits with file = /path/to/lib/cucumber/languages.yml"
task :update_scala_i18n, :file do |t, args|    
  # Limitations: Will only choose first variant of translation and will CamelCase multiple words 
  def e(str); str.gsub(/([\\'])|(\|.*)/, '').split(/[- _?()]/).map {|w| w.capitalize}.join; end 
  # Parse Cucumber language.yml and write the (new) Languages.scala file 
  out = File.new("cuke4duke/src/main/scala/cuke4duke/Languages.scala", "w")  
  out << "package cuke4duke\n"
  YAML.load_file(args.file).each do |key, lang|
    trait = [e(lang['name']), e(lang['given']), e(lang['when']), e(lang['then'])]
    # Avoid duplicate definitions of same keyword (see:Uzbek) and already defined English keywords
    unless trait.size != trait.uniq.size
      out << "trait #{trait[0]} { self: ScalaDsl =>\n"
      out << "  val #{trait[1]} = self.Given\n" unless trait[1] == 'Given'
      out << "  val #{trait[2]} = self.When\n" unless trait[2] == 'When'
      out << "  val #{trait[3]} = self.Then\n" unless trait[3] == 'Then'
      out << "}\n"
    else
      puts "Couldn't generate language: #{trait[0]}"
    end
  end
  out.close
end
