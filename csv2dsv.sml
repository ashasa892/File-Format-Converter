exception UnevenFields of string;

fun unevenfield_output(fieldCount, totalFields, row) = 
    "Found "^Int.toString(fieldCount)^" fields at line "^Int.toString(row)^": Expected "^Int.toString(totalFields)^"\n";

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn(infilename)
		val outs = TextIO.openOut(outfilename)
		val fieldCount : int ref = ref 0
		val totalFields : int ref = ref 0
		val row : int ref = ref 0;
		fun helper(curToken, isLastTokenEscape) = 
			case curToken of
				NONE => (
							TextIO.closeIn(ins); 
						 	TextIO.closeOut(outs)
						)
				| SOME(c) => (
								if c=delim1 then
									if isLastTokenEscape then
									(
										TextIO.output1(outs,c)
									)
									else
									(
										fieldCount := (!fieldCount)+1;
										TextIO.output1(outs,delim2)
									)
										
								else
								(
									if isLastTokenEscape then
									(
										TextIO.output1(outs,#"\\")
									)
									else if c = #"\n" then
									(
										fieldCount := (!fieldCount)+1;
										row := (!row)+1;
										if (!totalFields)=0 then
											(totalFields:=(!fieldCount); fieldCount:= 0)
										else if (!totalFields)<>(!fieldCount) then
											raise UnevenFields(unevenfield_output((!fieldCount), (!totalFields), (!row)))
										else fieldCount:=0
										
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
	end
		handle UnevenFields s => print("Exception UnevenFields: "^s) |
				Io => print("Exception FileNotFound: File to be converted not found\n");

