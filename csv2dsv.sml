exception UnevenFields of string;
exception QuoteMismatch;

(*Functino to print uneven field exception*)
fun unevenfield_output(fieldCount, totalFields, row) = 
    "Found "^Int.toString(fieldCount)^" fields at line "^Int.toString(row)^": Expected "^Int.toString(totalFields)^"\n";

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn(infilename)
		val outs = TextIO.openOut(outfilename)
		(*variables to store current field counts and current row*)
		val fieldCount : int ref = ref 0
		val totalFields : int ref = ref 0
		val row : int ref = ref 0;
		(*helper function to traverse the instream
		  curToken - current character in the input stream
		  quoteCount - counts of quotes in current field (it is decremented if quotes are matched and otherwise incremented (just like parenthesis))
		  quoteAdded - true if quote is manually added into beginning of the field in output file*)
		fun helper(curToken, quoteCount, quoteAdded) = 
			case curToken of
				(*if none then close the streams and also check for quote mismatch*)
				NONE => (
							if (quoteCount <> 0) then raise QuoteMismatch else ();
							TextIO.closeIn(ins); 
						 	TextIO.closeOut(outs)
						)
				| SOME(c) => (

								if c=delim1 then
									if (quoteCount=1 andalso quoteAdded = false) then (* if c=delim1 and quotes are mismatched (delim1 is part of field)*)
									(
										TextIO.output1(outs,c);
										helper(TextIO.input1(ins), quoteCount, quoteAdded)
									)
									else (*if c=delim1 and quotes are matched (field is completer)*)
									(
										if quoteAdded = true then TextIO.output1(outs,#"\"") else (); (*if quote is manually added, then, will have to manually match the quote*)
										fieldCount := (!fieldCount)+1; (*number of fields are incremented*)
										TextIO.output1(outs,delim2);
										helper(TextIO.input1(ins), 0, quoteAdded)
									)
										
								else
								(
									if c = #"\"" then (* if c is a quote *)
									(
										TextIO.output1(outs,c);
										if quoteCount = 0 then helper(TextIO.input1(ins), 1, false) (*all quotes are matched then it means that field has just begun*)
										else if quoteCount = 2 then helper(TextIO.input1(ins), 1, quoteAdded) (*already 2 quotes have occured in the filed (1 at beginning and other act as escaping quote for current quote) => mismatch quote is 1*)
										else helper(TextIO.input1(ins), quoteCount+1, quoteAdded) 
									)
									else if (c = #"\n" andalso (quoteCount=2 orelse quoteAdded)) then (*if endline and field is complete, then, row is over*)
									(
										if quoteAdded = true then TextIO.output1(outs,#"\"") else ();
										TextIO.output1(outs,c);
										fieldCount := (!fieldCount)+1;
										row := (!row)+1;

										if (!totalFields)=0 then
											(totalFields:=(!fieldCount); fieldCount:= 0)
										else if (!totalFields)<>(!fieldCount) then
											raise UnevenFields(unevenfield_output((!fieldCount), (!totalFields), (!row)))
										else fieldCount:=0;
										
										helper(TextIO.input1(ins), 0, false)
									) 
									else 
									(
										if quoteCount=0 then (*if input field does not quote at beginning then add manually*)
										(
											TextIO.output1(outs,#"\"");
											TextIO.output1(outs,c);
											helper(TextIO.input1(ins), 1, true)
										)
										else if quoteCount=2 then raise QuoteMismatch (*quote mismatch*)
										else  (*most normal case.. uffffff*)
										(
											TextIO.output1(outs,c);
											helper(TextIO.input1(ins), quoteCount, quoteAdded)
										)
									)

									
								)
								
								
							)
	in
		helper(TextIO.input1(ins), 0, false)
	end (*handle the exceptions*)
		handle UnevenFields s => print("Exception UnevenFields: "^s) |
			   QuoteMismatch => print("Exception QuoteMismatch: Quotes are not matched properly\n") |
			   Io => print("Exception FileNotFound: File to be converted not found\n");

