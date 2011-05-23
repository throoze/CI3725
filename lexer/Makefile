default: 
	alex src/Lexer.x -o bin/Lexer.hs	
	ghc --make -optl -w -o bin/vecti -odir bin -hidir bin bin/Lexer.hs src/Tokens.hs src/Main.hs
	

doc:	bin/Lexer.hs
	rm -rf doc
	haddock -h --odir=doc src/Lexer.hs src/Tokens.hs

clean:
	rm bin/*

cleanall:
	rm -f bin/* 2> /dev/null
	rm -f doc/* 2> /dev/null
	
