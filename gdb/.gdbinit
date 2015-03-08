define mono_backtrace
 select-frame 0
 set $i = 0
 while ($i < $arg0)
   set $foo = (char*) mono_pmip ($pc)
   if ($foo)
     printf "#%d %p in %s\n", $i, $pc, $foo
   else
     frame
   end
   up-silently
   set $i = $i + 1
 end
end
