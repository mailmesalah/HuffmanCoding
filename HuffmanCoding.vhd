----------------------------------------------------------------------
--------------------------//HUFFMAN CODING//--------------------------
------------------------************************----------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
-------------------------//////////////////---------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  
package Extras is 
  --generic ( Size : integer :=  4 ; 
            --MessageSize : integer :=  80 ) ;
  type datasize is array(0 to (19)) of std_logic_vector(0 to 7);
  type stringx is array(0 to 19) of string(1 to 20);
  type integerx is array(0 to 19) of integer;
  type datacount is array(0 to 19) of integer; 
  
  type Header is
    record
      Character : std_logic_vector(0 to 7) ;
      HuffmanCode : std_logic_vector(0 to 19) ;
      HuffmanCodeSize : integer ;
    end record ;
  type Head is array(0 to 19) of Header ;
end Extras;


library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
library HuffmanCoding;
  use HuffmanCoding.Extras.all;

entity HuffmanCoding is
  generic ( Size : integer :=  4 ; 
            MessageSize : integer :=  20 ) ;
  port( Input : in std_logic_vector(0 to 399);
        InLimit : in  integer ;
        InHeader : in Head ;
        InHeaderLimit : in integer ;
        --input table is needed
        Mode  : in  std_logic ;
        OutLimit :  out integer ;
        Output: out std_logic_vector(0 to 399);
        OutHeader : out Head ;
        OutHeaderLimit : out integer);
end HuffmanCoding;

architecture Operation of HuffmanCoding is
  
  shared variable i,j,SymbolsCount,NodeCount,Temp,CodeSize,CodeCount,Limit,Index: integer ;
  shared variable UniqueSymbols: datasize ;
  shared variable TempSymbol : std_logic_vector(0 to 7) ;
  shared variable Message : std_logic_vector(0 to 399) ;
  shared variable SymbolCount,TempCount,NodesCount,HuffmanCodeSize:datacount ;
  shared variable Found:std_logic;
  shared variable Tree,HuffmanCodes:stringx;
  shared variable TempCharacter,Code:string(1 to 20);
  shared variable TempInput : datasize ;
  shared variable TempHeader : Head ;
  shared variable TempCode: std_logic_vector(0 to 19) ;
  --shared variable
 
begin
  process(Input,InLimit,Mode)
    
    function StringToStdLogic( a:string;b:integer)
                            return std_logic_vector is
    --Function to convert String to std_logic_vector
    --only zero and one is converted
      
      variable x:std_logic_vector(0 to b-1);
      variable ReturnValue:std_logic_vector(0 to b-1); 
      
      begin
          
        for i in 1 to b loop
          case a(i) is
            when '0'=> x(i-1):='0';
            when '1'=> x(i-1):='1';
            when others =>null;
          end case ;
        end loop ;         
        
        ReturnValue:=x;
          
        return ReturnValue;
    end StringToStdLogic;
    
    begin
      
      if(Mode = '0')  then  --  Compressing
        
        --change input stream to input table(array of characters)
        for i in 0 to (InLimit-1) loop
          TempInput(i):=Input((i*8) to ((i*8)+7)) ;
        end loop ;
        
        --Removing the duplicate symbols from input
        SymbolsCount := 0 ;
        
        for i in 0 to (InLimit-1) loop
          Found := '0';
          for j in 0 to (SymbolsCount-1) loop
            if(TempInput(i) = UniqueSymbols(j))then
              Found := '1' ;
            end if;
          end loop ;
          if(Found = '0')then
            SymbolsCount := SymbolsCount + 1;
            UniqueSymbols((SymbolsCount-1)) := TempInput(i) ;
          end if;
        end loop ;
        
        --Setting Counts to 0
        for i in 0 to (MessageSize-1) loop
          SymbolCount(i) := 0 ;
        end loop;
        
        --Counting each Symbols' count
        for i in 0 to (SymbolsCount-1) loop
          
          for j in 0 to (InLimit-1) loop
            if(UniqueSymbols(i) = TempInput(j))then
              SymbolCount(i) := SymbolCount(i) + 1 ;
            end if;
          end loop ;
  
        end loop ;
        
        --creating the Huffman tree......
        
        --clearing the Tree
        for i in 0 to (MessageSize-1) loop
          Tree(i) := "00000000000000000000" ;
          NodesCount(i) := 1;
        end loop ;
        
        --setting the characters used
        --for i in 1 to SymbolsCount loop
        Tree(0) := "A0000000000000000000" ;
        Tree(1) := "B0000000000000000000" ;
        Tree(2) := "C0000000000000000000" ;
        Tree(3) := "D0000000000000000000" ;
        Tree(4) := "E0000000000000000000" ;
        Tree(5) := "F0000000000000000000" ;
        Tree(6) := "G0000000000000000000" ;
        Tree(7) := "H0000000000000000000" ;
        Tree(8) := "I0000000000000000000" ;
        Tree(9) := "J0000000000000000000" ;
        Tree(10) := "K0000000000000000000" ;
        Tree(11) := "L0000000000000000000" ;
        Tree(12) := "M0000000000000000000" ;
        Tree(13) := "N0000000000000000000" ;
        Tree(14) := "O0000000000000000000" ;
        Tree(15) := "P0000000000000000000" ;
        Tree(16) := "Q0000000000000000000" ;
        Tree(17) := "R0000000000000000000" ;
        Tree(18) := "S0000000000000000000" ;
        Tree(19) := "T0000000000000000000" ;
        --end loop ;
        
        --the character count is used to form the Tree (Node)
        NodeCount := SymbolsCount ; --number of characters
        TempCount := SymbolCount ;  --count of each characters
        
        --creating the equation
        while ( NodeCount > 1 ) loop
          
          --sort characters according to their count (Descending)
          for i in 0 to (NodeCount-2) loop
            for j in i+1 to (NodeCount-1) loop
              
              if(TempCount(i)>TempCount(j))then
                
                --swapping TempCount
                Temp := TempCount(i) ;
                TempCount(i) := TempCount(j) ;
                TempCount(j) := Temp ;
                
                --swapping Tree character
                TempCharacter := Tree(i) ;
                Tree(i) := Tree(j) ;
                Tree(j) := TempCharacter ;
                
                --swaping nodescount
                Temp := NodesCount(i) ;
                NodesCount(i) := NodesCount(j) ;
                NodesCount(j) := Temp ;
                
              end if;
            end loop ;
          end loop ;
          
          --merging characters
          Temp := NodesCount(0) + NodesCount(1) + 3 ;
          TempCharacter := "00000000000000000000" ;
          
          Tree(1):="(" & Tree(0)(1 to NodesCount(0)) & "+" & Tree(1)(1 to NodesCount(1)) & ")" & TempCharacter(1 to (20-Temp)) ;
          TempCount(1) := TempCount(0) + TempCount(1) ;
          NodesCount(1):= NodesCount(0) + NodesCount(1) + 3 ;
          --Rearranging
          for i in 0 to NodeCount-1 loop
            Tree(i):= Tree(i+1) ;
            TempCount(i) := TempCount(i+1) ;
            NodesCount(i) := NodesCount(i+1) ;
          end loop ;
          --decrementing NodeCount
          NodeCount := NodeCount - 1 ;
          
        end loop;
        
        --Tree is created...
        
        TempCharacter := Tree(0) ;
        --Finding the Haffman code for each characters in the message from the Tree created
        Code := "XXXXXXXXXXXXXXXXXXXX" ;
        CodeSize := 0;
        CodeCount := 0;
        for i in 1 to NodesCount(0) loop
          
          if(TempCharacter(i)='(')then
            CodeSize := CodeSize + 1 ;
            Code (CodeSize)  := '0' ; 
          elsif(TempCharacter(i)=')')then
            CodeSize := CodeSize - 1 ;
          elsif(TempCharacter(i)='+')then
            Code (CodeSize)  := '1' ; 
          else
            
            HuffmanCodes(CodeCount)(1 to CodeSize) := Code(1 to CodeSize) ;
            HuffmanCodeSize(CodeCount) := CodeSize ;              
            CodeCount := CodeCount + 1 ;
          end if ;
        end loop ;
        
        --Huffman code is now available
        
        --Sort it with the unique symbol for applying the code for characters
        
        --sorting Huffman Code
        for i in 0 to CodeCount-2 loop
          for j in i+1 to CodeCount-1 loop
            if(HuffmanCodeSize(i)>HuffmanCodeSize(j))then
              --sorting the code
              TempCharacter(1 to HuffmanCodeSize(i)) := HuffmanCodes(i)(1 to HuffmanCodeSize(i)) ;
              HuffmanCodes(i)(1 to HuffmanCodeSize(j)) := HuffmanCodes(j)(1 to HuffmanCodeSize(j)) ;
              HuffmanCodes(j)(1 to HuffmanCodeSize(i)) := TempCharacter ;
              --sorting the size
              Temp := HuffmanCodeSize(i) ;
              HuffmanCodeSize(i) := HuffmanCodeSize(j) ;
              HuffmanCodeSize(j) := Temp ;
            end if ;
          end loop ;
        end loop ;
        
        --sorting UniqueSymbol (Descending)
        for i in 0 to SymbolsCount-2 loop
          for j in i+1 to SymbolsCount-1 loop
            if(SymbolCount(i)<SymbolCount(j))then
              --swap
              Temp := SymbolCount(i) ;
              SymbolCount(i) := SymbolCount(j) ;
              SymbolCount(j) := Temp ;
              
              TempSymbol := UniqueSymbols(i) ;
              UniqueSymbols(i) := UniqueSymbols(j) ;
              UniqueSymbols(j) := TempSymbol ;
            end if ;
          end loop ;
        end loop ; 
        
        --creating the Encoded Message....
        Index := 0 ;
        for i in 0 to InLimit-1 loop
          for j in 0 to SymbolsCount-1 loop
            if(TempInput(i)=UniqueSymbols(j))then
              Message(Index to (Index+(HuffmanCodeSize(j)-1))):=StringToStdLogic(HuffmanCodes(j),HuffmanCodeSize(j));
              Index := Index+HuffmanCodeSize(j) ;
            end if ;
          end loop ;
        end loop ;
        
        --creating the Header data
        for i in 0 to SymbolsCount-1 loop
          TempHeader(i).Character := UniqueSymbols(i);
          TempHeader(i).HuffmanCode(0 to (HuffmanCodeSize(i)-1)) := StringToStdLogic(HuffmanCodes(i)(1 to (HuffmanCodeSize(i))),HuffmanCodeSize(i));
          TempHeader(i).HuffmanCodeSize := HuffmanCodeSize(i);
        end loop;
        
        --Now the Outputs are available
        OutHeader <=TempHeader ;
        OutHeaderLimit <= SymbolsCount ;
        Output <= Message ; 
        OutLimit <= Index ;
        --end of Compression
      elsif(Mode = '1') then   --  Decompressing
        
        TempCode := "UUUUUUUUUUUUUUUUUUUU" ;
        CodeSize := 0;
        CodeCount := 0;--counts the number of characters
        Index := 0 ;
        for i in 0 to (Inlimit-1) loop
          TempCode(CodeSize) := Input(i) ;
          CodeSize := CodeSize + 1 ;
          for j in 0 to (InHeaderLimit-1) loop
            if(InHeader(j).HuffmanCode(0 to (InHeader(j).HuffmanCodeSize - 1)) = TempCode(0 to (CodeSize-1)))then
              Message(Index to (Index + 7)) := InHeader(j).Character ;
              Index := Index + 8 ;
              CodeCount := CodeCount + 1 ; 
              CodeSize := 0 ;
            end if;
          end loop;
        end loop ;
      end if;
  
      --Now the Outputs are available
      --OutHeader <=TempHeader ;
      --OutHeaderLimit <= SymbolsCount ;--this is not needed for decompression
      Output <= Message ; 
      OutLimit <= CodeCount ;
      --end of decompression
    end process;
end Operation;
