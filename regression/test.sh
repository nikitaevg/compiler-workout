make TOPFILE=test000 -B
./test000.opt > test000.log
diff test000.log orig/test000.log
