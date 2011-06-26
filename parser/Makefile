default: 
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs -iprueba
	ghc --make -optl -w -o bin/vecti -odir bin -hidir bin src/Lexer.hs src/Parser.hs src/Tokens.hs src/Main.hs
	

docs:	
	rm -rf doc
	haddock -h --odir=doc src/Lexer.hs src/Tokens.hs

clean:
	rm bin/* bin/Parser.hs bin/Lexer.hs

cleanall:
	rm -f bin/* 2> /dev/null
	rm -f doc/* 2> /dev/null
	
