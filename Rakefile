sources = ['.emacs', '.emacs.d', '.screenrc', '.zshrc']

IGNORE_LIST = ['.', '..', '.svn', 'auto-save-list', 'anything-c-adaptive-history']

src, dest =
  if RUBY_PLATFORM.match('mswin32')
    [ sources.select {|s| s.match('\.emacs')},
      'C:/ProgramFiles/meadow/', ]
  else
    [ sources, ENV['HOME'], ]
  end


class MyRakemethod

  def self.install(src, dest)
    mkdir_p(dest)
    if FileUtils.uptodate?(dest, src)
      raise "#{dest} is newer than #{src}\n" +
            " [#{File.mtime(dest)}] / [#{File.mtime(src)}]\n"
    else
       FileUtils.install(src, dest,
                              { :preserve => true, :verbose => true } )
    end
  end

  def self.cp_r(src, dest)
    mkdir_p(dest)
    FileUtils.cp(src, dest, { :preserve => true, :verbose => true } )
  end

  def self.mkdir_p(dest)
    dirname = File.dirname(dest)
    FileUtils.mkdir_p(dirname) unless File.exists?(dirname)
  end

end


def dir_walk(src, dest, method)
  dest += '/' + File.basename(src)
  case File.ftype(src)
  when 'file'
    MyRakemethod.send(method, src, dest)

  when 'directory'
    Dir.foreach(src) do |e|
      next if IGNORE_LIST.any? { |i| i == e }

      dir_walk( src + '/' + e, dest, method)
     end

  else
    raise "Cannot handle the filetype: #{src} [#{File.ftype(src)}]"
  end
end


#
# コピー元の方が新しくて、かつコピー元とコピー先の内容が違う場合に
# ファイルのコピーを行う
#
task :install do
  src.each { |s| dir_walk(s, dest, :install) }
  puts 'Finish!!'
end

#
# コピー元とコピー先の内容を考慮せずにコピーを行う
#
task :copy do
  src.each { |s| dir_walk(s, dest, :cp_r) }
  puts 'Finish!!'
end

#
# ':install' と、コピー元とコピー先を逆にして、
# 同様の動作を行う
#
task :r_install do
  src.each { |s| dir_walk(dest + s, '.', :install) }
  puts 'Finish!!'
end

#
# ':copy' と、コピー元とコピー先を逆にして、
# 同様の動作を行う
#
task :r_copy do
  src.each { |s| dir_walk(dest + s, '.', :cp_r) }
  puts 'Finish!!'
end
