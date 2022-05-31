convertBinToDec(Bin,Dec):-
	convertBinToDecAcc(Bin,Dec,0).
convertBinToDecAcc(0,0,_).
convertBinToDecAcc(Bin,Dec,Acc):-
	Bin\=0,
	Rbit is Bin mod 10,
	Bin2 is Bin//10,
	Acc2 is Acc+1,
	convertBinToDecAcc(Bin2,Dec2,Acc2),
	Tempo is 2**Acc,
	Tempo2 is Tempo*Rbit,
	Dec is  Tempo2+Dec2.	 


replaceIthItem(_,[],_,[]).
replaceIthItem(Item,[_|T],0,[Item|T]).
replaceIthItem(Item,[H|T],I,Result):-
	I\=0,
	I1 is I-1,
	replaceIthItem(Item,T,I1,R1),
	Result = [H|R1].


splitEvery(N,List,Res):-
	splitEveryAcc(N,List,Res,[],0,[]).
splitEveryAcc(N,[H|T],Res,AccRes,Counter,Acc):-
	Counter<N,
	append(Acc,[H],NewAcc),
	NewCounter is Counter+1,
	splitEveryAcc(N,T,Res,AccRes,NewCounter,NewAcc).
splitEveryAcc(N,[H|T],Res,AccRes,Counter,Acc):-
	Counter=N,
	append(AccRes,[Acc],AccRes2),
	splitEveryAcc(N,[H|T],Res,AccRes2,0,[]).
splitEveryAcc(_,[],Res,AccRes,_,Acc):-
	append(AccRes,[Acc],Res).


logBase2(1,0).
logBase2(Num,Res):-
	Num>1,
	Num2 is Num/2,
	logBase2(Num2,Res2),
	Res is Res2+1.
	

fillZeros(String,0,String).
fillZeros(String,N,R):-
	N>0,
	N1 is N-1,
	string_concat("0",String,R1),
	fillZeros(R1,N1,R).

getNumBits(_,fullyAssoc,_,0).
getNumBits(X,setAssoc,_,BitsNum):-
	logBase2(X,BitsNum).
getNumBits(_,directMap,Cache,BitsNum):-
	length(Cache,X),
	logBase2(X,BitsNum).

getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
	X is 10**BitsNum,
	\+number(StringAddress),
	atom_number(StringAddress,IntAddress),
	IntAddress2 is IntAddress mod X,
	convertBinToDec(IntAddress2,Numb),
	IntAddress3 is IntAddress//X,
	atom_number(NewStringAddress,IntAddress3),
	getDataFromCacheHelp(NewStringAddress,Cache,Data,HopsNum,directMap,BitsNum,Numb).


getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
	X is 10**BitsNum,
	number(StringAddress),
	IntAddress = StringAddress,
	IntAddress2 is IntAddress mod X,
	convertBinToDec(IntAddress2,Numb),
	IntAddress3 is IntAddress//X,
	atom_number(NewStringAddress,IntAddress3),
	getDataFromCacheHelp(NewStringAddress,Cache,Data,HopsNum,directMap,BitsNum,Numb).


getDataFromCacheHelp(StringAddress,[_|T],Data,HopsNum,directMap,BitsNum,Numb):-
	Numb>0,
	Numb2 is Numb-1,
	getDataFromCacheHelp(StringAddress,T,Data,HopsNum,directMap,BitsNum,Numb2).
getDataFromCacheHelp(StringAdress,[H|_],Data,HopsNum,directMap,_,Numb):-
	Numb=0,
	HopsNum=0,
	H = item(tag(X),data(Data),1,_),
	number(X),
	number(StringAdress),
	X = StringAdress.

getDataFromCacheHelp(StringAdress,[H|_],Data,HopsNum,directMap,_,Numb):-
	Numb=0,
	HopsNum=0,
	H = item(tag(X),data(Data),1,_),
	\+number(X),	
	atom_number(X,J),
	\+number(StringAdress),
	atom_number(StringAdress,J).

getDataFromCacheHelp(StringAdress,[H|_],Data,HopsNum,directMap,_,Numb):-
	Numb=0,
	HopsNum=0,
	H = item(tag(X),data(Data),1,_),
	\+number(X),
	number(StringAdress),
	atom_number(X,J),
	J = StringAdress.

getDataFromCacheHelp(StringAdress,[H|_],Data,HopsNum,directMap,_,Numb):-
	Numb=0,
	HopsNum=0,
	H = item(tag(X),data(Data),1,_),
	number(X),
	\+number(StringAdress),
	atom_number(StringAdress,J),
	X = J.


convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
	convertAddressHelp(Bin,BitsNum,Tag,Idx,directMap,0).
convertAddressHelp(Bin,0,Bin,0,directMap,_).
convertAddressHelp(Bin,BitsNum,Tag,Idx,directMap,Count):-
	BitsNum\=0,
	X is Bin mod 10,
	NewBin is Bin//10,
	NewBitsNum is BitsNum-1,
	NewCount is Count+1,
	convertAddressHelp(NewBin,NewBitsNum,Tag,Idx2,directMap,NewCount),
	Z is 10**Count,
	Y is X*Z,
	Idx is Idx2+Y.

lengthOfNumber(X,1):-
	X>=0,
	X=<9.

lengthOfNumber(X,Y):-
	X>=10,
	X1 is X // 10,
	lengthOfNumber(X1,Y1),
	Y is Y1+1.

replaceInCacheHelp(0,[H|_],H).
	
replaceInCacheHelp(Num,[_|T],Value):-
	Num>0,
	Num2 is Num -1,
	replaceInCacheHelp(Num2,T,Value).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
	atom_number(STag,Tag),
	atom_number(SIdx,Idx),
	lengthOfNumber(Idx,X),
	Need is BitsNum-X,
	fillZeros(SIdx,Need,SIdx2),
	string_concat(STag,SIdx2,STot),
	atom_number(STot,Tot),
	convertBinToDec(Tot,Num),
	replaceInCacheHelp(Num,Mem,ItemData),
	convertBinToDec(Idx,Num2),
	replaceInCacheAcc(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum,[],Num2).

replaceInCacheAcc(Tag,_,_,[_|T],NewCache,ItemData,directMap,BitsNum,Acc,0):-
	lengthOfNumber(Tag,Len),
	Need2 is 6-Len-BitsNum,
	atom_number(A,Tag),
	fillZeros(A,Need2,NewTag),
	append(Acc,[item(tag(NewTag),data(ItemData),1,0)],Acc2),
	append(Acc2,T,NewCache).

replaceInCacheAcc(Tag,Idx,Mem,[H|T],NewCache,ItemData,directMap,BitsNum,Acc,Num):-
	Num\=0,
	Num2 is Num-1,
	append(Acc,[H],NewAcc),
	replaceInCacheAcc(Tag,Idx,Mem,T,NewCache,ItemData,directMap,BitsNum,NewAcc,Num2).

getSet([H|T],Index,NumInSet,Set,NumInSet,Acc,NewCache):-
	Set2 is Set+1,
	getSet([H|T],Index,NumInSet,Set2,0,Acc,NewCache).

getSet([H|T],Index,NumInSet,Set,Count,Acc,NewCache):-
	Count<NumInSet,
	Set=Index,
	append(Acc,[H],NewAcc),
	Count2 is Count+1,
	getSet(T,Index,NumInSet,Set,Count2,NewAcc,NewCache).

getSet([_|T],Index,NumInSet,Set,Count,Acc,NewCache):-
	Count<NumInSet,
	Set\=Index,
	Count2 is Count+1,
	getSet(T,Index,NumInSet,Set,Count2,Acc,NewCache).

getSet([],_,_,_,_,Acc,Acc).

hopHelp([H|T],HopsNum,Acc,Tag,Data):-
	H = item(tag(X),data(Y),_,_),
	\+atom_number(X,Tag),
	Y\=Data,
	Acc2 is Acc+1,
	hopHelp(T,HopsNum,Acc2,Tag,Data).
hopHelp([H|T],HopsNum,Acc,Tag,Data):-
	H = item(tag(X),data(Y),_,_),
	atom_number(X,Tag),
	Y\=Data,
	Acc2 is Acc+1,
	hopHelp(T,HopsNum,Acc2,Tag,Data).
hopHelp([H|T],HopsNum,Acc,Tag,Data):-
	H = item(tag(X),data(Y),_,_),
	\+atom_number(X,Tag),
	Y=Data,
	Acc2 is Acc+1,
	hopHelp(T,HopsNum,Acc2,Tag,Data).

hopHelp([H|_],Acc,Acc,Tag,Data):-
	H = item(tag(X),data(Y),_,_),
	atom_number(X,Tag),
	Y=Data.

dataget([H|_],FinalTag,Data):-
	H = item(tag(TT),data(XX),1,_),
	TT = FinalTag,
	Data =XX.

dataget([H|T],FinalTag,Data):-
	H = item(tag(TT),data(_),_,_),
	TT \= FinalTag,
	dataget(T,FinalTag,Data).



getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	logBase2(SetsNum,BitsNum),
	\+number(StringAddress),
	X is 10**BitsNum, 
	atom_number(StringAddress,IntAddress),
	Index is IntAddress mod X,
	convertBinToDec(Index,Index2),
	Tag is IntAddress // X,
	length(Cache,Len),
	NumInSet is Len/SetsNum,
	getSet(Cache,Index2,NumInSet,0,0,[],NewCache),
	lengthOfNumber(Tag,KK),
	Ned is 6 - BitsNum-KK,
	atom_number(StringTag,Tag),
	fillZeros(StringTag,Ned,FinalTag),
	dataget(NewCache,FinalTag,Data),
	hopHelp(NewCache,HopsNum,0,Tag,Data).


getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	logBase2(SetsNum,BitsNum),
	number(StringAddress),
	IntAddress = StringAddress,
	X is 10**BitsNum, 
	Index is IntAddress mod X,
	convertBinToDec(Index,Index2),
	Tag is IntAddress // X,
	length(Cache,Len),
	NumInSet is Len/SetsNum,
	getSet(Cache,Index2,NumInSet,0,0,[],NewCache),
	lengthOfNumber(Tag,KK),
	Ned is 6 - BitsNum-KK,
	atom_number(StringTag,Tag),
	fillZeros(StringTag,Ned,FinalTag),
	dataget(NewCache,FinalTag,Data),
	hopHelp(NewCache,HopsNum,0,Tag,Data).


convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
	logBase2(SetsNum,BitsNeeded),
	X is 10**BitsNeeded,
	Idx is Bin mod X,
	Tag is Bin // X.

helpFIFO([],X,X,Tag,Tag).

helpFIFO([item(tag(TAG),_,_,S)|T],P,R,Tag,_):-
	S>=R,
	helpFIFO(T,P,S,Tag,TAG).

helpFIFO([item(_,_,_,S)|T],P,R,Tag,Help):-
	S<R,
	helpFIFO(T,P,R,Tag,Help).
	




gotZeros([item(_,_,0,_)|_]).
gotZeros([item(_,_,1,_)|T]):-
	gotZeros(T).

helpzeroagain(Tag,Idx,[item(A,B,1,D)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentIndex<NuminSet,
	CurrentSet = Index,
	D1 is D+1,
	append(Acc,[item(A,B,1,D1)],NewAcc),
	Ind2 is CurrentIndex+1,
	helpzeroagain(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Ind2,NewAcc).

helpzeroagain(Tag,Idx,[item(A,B,0,D)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentIndex<NuminSet,
	CurrentSet = Index,
	append(Acc,[item(A,B,0,D)],NewAcc),
	Ind2 is CurrentIndex+1,
	helpzeroagain(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Ind2,NewAcc).


helpzeroagain(_,_,L,NewCache,_,NuminSet,_,_,CurrentIndex,Acc):-
	CurrentIndex = NuminSet,
	append(Acc,L,NewCache).
	
	

helpzero(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet,XX,Acc):-
	XX = NuminSet,
	CurrentSet2 is CurrentSet+1,
	helpzero(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet2,0,Acc).

helpzero(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentSet<Index,
	CurrentIndex<NuminSet,
	append(Acc,[H],NewAcc),
	Index2 is CurrentIndex+1,
	helpzero(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Index2,NewAcc).

helpzero(Tag,Idx,[item(_,_,0,_)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentSet=Index,
	CurrentIndex \= NuminSet,
	atom_number(Stag,Tag),
	lengthOfNumber(Tag,X),
	logBase2(Idx,A7),
	Need is 6-A7-X,
	fillZeros(Stag,Need,NewTag),
	Ind2 is CurrentIndex+1,
	append(Acc,[item(tag(NewTag),data(ItemData),1,0)],Final),
	helpzeroagain(Tag,Idx,T,NewCache,itemData,NuminSet,Index,CurrentSet,Ind2,Final).

helpzero(Tag,Idx,[item(_,_,0,_)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentSet=Index,
	CurrentIndex = NuminSet,
	atom_number(Stag,Tag),
	lengthOfNumber(Tag,X),
	logBase2(Idx,A7),
	Need is 6-A7-X,
	fillZeros(Stag,Need,NewTag),
	append(Acc,[item(tag(NewTag),data(ItemData),1,0)],Final),
	append(Final,T,NewCache).

helpzero(Tag,Idx,[item(A,B,1,D)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc):-
	CurrentSet=Index,
	D1 is D+1,
	Q = item(A,B,1,D1),
	append(Acc,[Q],NewAcc),
	Index2 is CurrentIndex+1,
	helpzero(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Index2,NewAcc).


helpme(Tag,Idx,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
	length(OldCache,Len),
	NumInSet is Len/SetsNum,
	convertBinToDec(Idx,Index),
	getSet(OldCache,Index,NumInSet,0,0,[],Cache2),
	gotZeros(Cache2),
	helpzero(Tag,SetsNum,OldCache,NewCache,ItemData,NumInSet,Index,0,0,[]).


helpme(Tag,Idx,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
	length(OldCache,Len),
	NumInSet is Len/SetsNum,
	convertBinToDec(Idx,Index),
	getSet(OldCache,Index,NumInSet,0,0,[],Cache2),
	\+gotZeros(Cache2),
	helpFIFO(Cache2,G,0,TAG,7),
	helploop(Tag,SetsNum,OldCache,NewCache,ItemData,NumInSet,Index,0,0,[],G,TAG).

	



replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
	atom_number(STag,Tag),
	atom_number(SIdx,Idx),
	lengthOfNumber(Idx,X),
	logBase2(SetsNum,BitsNum),
	Need is BitsNum-X,
	fillZeros(SIdx,Need,SIdx2),
	string_concat(STag,SIdx2,STot),
	atom_number(STot,Tot),
	convertBinToDec(Tot,Num),
	replaceInCacheHelp(Num,Mem,ItemData),
	helpme(Tag,Idx,OldCache,NewCache,ItemData,setAssoc,SetsNum).
	


helploop(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet,XX,Acc,G,TAG):-
	XX = NuminSet,
	CurrentSet2 is CurrentSet+1,
	helploop(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet2,0,Acc,G,TAG).

helploop(Tag,Idx,[H|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc,G,TAG):-
	CurrentSet<Index,
	CurrentIndex<NuminSet,
	append(Acc,[H],NewAcc),
	Index2 is CurrentIndex+1,
	helploop(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Index2,NewAcc,G,TAG).

helploop(Tag,Idx,[item(tag(HH),_,K,_)|T],NewCache,ItemData,_,Index,CurrentSet,_,Acc,G,TAG):-
	CurrentSet=Index,
	HH= TAG,
	K =G,
	atom_number(Stag,Tag),
	lengthOfNumber(Tag,X),
	logBase2(Idx,A7),
	Need is 6-A7-X,
	fillZeros(Stag,Need,NewTag),
	append(Acc,[item(tag(NewTag),data(ItemData),1,0)],Final),
	append(Final,T,NewCache).




helploop(Tag,Idx,[item(tag(HH),BO,1,J)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc,G,TAG):-
	CurrentSet=Index,
	CurrentIndex\=NuminSet,
	HH\= TAG,
	J1 is J+1,
	Ind2 is CurrentIndex+1,
	append(Acc,[item(HH),BO,1,J1],Final),
	helploop(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Ind2,Final,G,TAG).


helploop(Tag,Idx,[item(tag(HH),BO,0,J)|T],NewCache,ItemData,NuminSet,Index,CurrentSet,CurrentIndex,Acc,G,TAG):-
	CurrentSet=Index,
	CurrentIndex\=NuminSet,
	HH\= TAG,
	Ind2 is CurrentIndex+1,
	append(Acc,[item(HH),BO,0,J],Final),
	helploop(Tag,Idx,T,NewCache,ItemData,NuminSet,Index,CurrentSet,Ind2,Final,G,TAG).



helploop(_,_,L,NewCache,_,NuminSet,Index,CurrentSet,CurrentIndex,Acc,_,_):-
	CurrentSet=Index,
	CurrentIndex=NuminSet,
	append(Acc,L,NewCache).



runProgram([],OldCache,_,OldCache,[],[],_,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
	getNumBits(NumOfSets,Type,OldCache,BitsNum),
	(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
	getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
	runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).


%------------------------------------------------------------------------------------------------

getData(StringAddress,OldCache,_,NewCache,Data,HopsNum,Type,BitsNum,hit):-
	getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
	\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	atom_number(StringAddress,Address),
	convertAddress(Address,BitsNum,Tag,Idx,Type),
	replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).
%--------------------------------------------------------------------------

getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,BitsNum):-
	getDataFromCacheHelp(StringAddress,Cache,Data,HopsNum,fullyAssoc,BitsNum,0).
getDataFromCacheHelp(StringAddress,[item(tag(StringAddress),data(Data),1,_)|_],Data,Hops,fullyAssoc,BitsNum,Hops).
getDataFromCacheHelp(StringAddress,[item(tag(X),_,_,_)|T],Data,HopsNum,fullyAssoc,BitsNum,Hops):-
	StringAddress \= X,
	Hops2 is Hops + 1,
	getDataFromCacheHelp(StringAddress,T,Data,HopsNum,fullyAssoc,BitsNum,Hops2).

convertAddress(Bin,BitsNum,Bin,Idx,fullyAssoc).	

concatZeros(T,T):-	
	string_length(T,6).
concatZeros(Tag,R):-
	string_length(Tag,L),
	L\=6,
	string_concat("0",Tag,Tag2),
	concatZeros(Tag2,R).

allOne([]).
allOne([item(_,_,1,_)|T]):-
	allOne(T).
	
getFirstIn([],Y,Y).	
getFirstIn([item(_,_,_,X)|T],Y,R):-
	Y>X,
	getFirstIn(T,Y,R).
getFirstIn([item(_,_,_,X)|T],Y,R):-
	X>Y,
	getFirstIn(T,X,R).	
		

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
	convertBinToDec(Tag,Num),
	replaceInCacheHelp(Num,Mem,ItemData),
	\+ allOne(OldCache),
	replaceInCacheHelperNotOnes(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum).
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
	convertBinToDec(Tag,Num),
	replaceInCacheHelp(Num,Mem,ItemData),
	allOne(OldCache),
	getFirstIn(OldCache,0,R),
	replaceInCacheHelperAllOnes(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum,R).		
	
replaceInCacheHelperNotOnes(_,_,_,[],[],_,_,_,_).
replaceInCacheHelperNotOnes(Tag,Idx,Mem,[item(_,_,0,_)|T],[X|NewCache],ItemData,fullyAssoc,BitsNum):-
	concatZeros(Tag,NewTag),
	X = item(tag(NewTag),data(ItemData),1,0),
	replaceInCacheIncRest(T,NewCache).
replaceInCacheHelperNotOnes(Tag,Idx,Mem,[item(tag(X),data(Y),1,Z)|T],[item(tag(X),data(Y),1,Z2)|NewCache],ItemData,fullyAssoc,BitsNum):-
	Z2 is Z + 1,
	replaceInCacheHelperNotOnes(Tag,Idx,Mem,T,NewCache,ItemData,fullyAssoc,BitsNum).
	
replaceInCacheHelperAllOnes(_,_,_,[],[],_,_,_,_).
replaceInCacheHelperAllOnes(Tag,Idx,Mem,[item(_,_,_,R)|T],[X|NewCache],ItemData,fullyAssoc,BitsNum,R):-
	concatZeros(Tag,NewTag),
	X = item(tag(NewTag),data(ItemData),1,0),
	replaceInCacheHelperAllOnes(Tag,Idx,Mem,T,NewCache,ItemData,fullyAssoc,BitsNum,R).
replaceInCacheHelperAllOnes(Tag,Idx,Mem,[item(tag(X),data(Y),Z,NotR)|T],[item(tag(X),data(Y),Z,NotR2)|NewCache],ItemData,fullyAssoc,BitsNum,R):-	
	NotR \= R,
	NotR2 is NotR + 1,
	replaceInCacheHelperAllOnes(Tag,Idx,Mem,T,NewCache,ItemData,fullyAssoc,BitsNum,R).


replaceInCacheIncRest([],[]).
replaceInCacheIncRest([item(tag(X),data(Y),1,Z)|T],[item(tag(X),data(Y),1,Z2)|R]):-
	Z2 is Z+1,
	replaceInCacheIncRest(T,R).
replaceInCacheIncRest([item(tag(X),data(Y),0,Z)|T],[item(tag(X),data(Y),0,Z)|R]):-
	replaceInCacheIncRest(T,R).