default: 
	alex src/Lexer.x -o bin/Lexer.hs	
	ghc --make -optl -w -o bin/vecti -odir bin -hidir bin bin/Lexer.hs src/Tokens.hs src/Main.hs
	

doc:
	haddock -h src/Token.hs -o doc bin/Lexer.hs 

clean:
	rm bin/*

cleanall:
	rm bin/*
	rm doc/*
	
