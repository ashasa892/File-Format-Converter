fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn(infilename)
		val outs = TextIO.openOut(outfilename)
		fun helper(curToken, isLastTokenIsEscape) = 
			case curToken of
				NONE => (
							TextIO.closeIn(ins); 
						 	TextIO.closeOut(outs)
						)
				| SOME(c) => (
								if c=delim1 then
									if isLastTokenIsEscape then
									(
										isLastTokenIsEscape = false;
										TextIO.output1(outs,c)
									)
									else
										TextIO.output1(outs,delim2)
								else
								(
									if isLastTokenIsEscape then
									(
										isLastTokenIsEscape = false;
										TextIO.output1(outs,#"\\")
									)
									else ();

									if c=delim2 then
										TextIO.output1(outs,#"\\")
									else ();

									if c <> #"\\" then
										TextIO.output1(outs,c)
									else ()
								);
								
								if c = #"\\" then
									helper(TextIO.input1(ins), true)
								else
									helper(TextIO.input1(ins), false)
							)
	in
		helper(TextIO.input1(ins), false)
	end;
