//
//Huffman.fs
//
//Huffman Program in FSharp
//for encoding and decoding
//through Huffman Tree and Huffman Table.
//
//@author Huy Tu
//@date 11/21/2014
//

module Huffman
    //explode function pulls out characters
    //from the string and return a list of characters
    //explode "CAT" returns ['C';'A';'T']
    let explode (s:string) =
        [for c in s -> c]

    //encodeChar is a recursive function that
    //does a lookup in the huffmanTable for the char 
    //and returns the associated binary string
    let rec encodeChar lst ch =
        match lst with
        |[] -> ""
        |h::t -> if (fst h) = ch then snd h
                 else encodeChar t ch  
    
    //huffmanTable have the character of alphabet letter
    //and its' corresponding binary string
    let huffmanTable = 
     [ ('A',"1011");   ('B',"100000");   ('C',"01000");('D',"10101");    ('E',"110");
      ('F',"00000");  ('G',"100011");   ('H',"0110"); ('I',"1111");     ('J',"000011011");
      ('K',"0000111");('L',"10100");    ('M',"00011");('N',"1110");     ('O',"1001");
      ('P',"100001"); ('Q',"000011001");('R',"0101"); ('S',"0111");     ('T',"001");
      ('U',"01001");  ('V',"000010");   ('W',"00010");('X',"000011010");('Y',"100010");
      ('Z',"000011000")]
    
    //encode function explodes input string into char list,
    //recurses on char list, and append those binary strings
    //together
    let encode str =
        let rec func lst = 
            match lst with
            |[]->""
            |h::t -> (encodeChar huffmanTable h) + (func t)    
        func (explode str) 

    //Create the Huffman Tree
    //of the hierarchy between the letter alphabets
    //for encodeChar to look up.
    type huffmanTreeType =
        | Leaf of char
        | Node of huffmanTreeType * huffmanTreeType
    let huffmanTree =
       Node(Node(Node(Node(Node(Leaf('F'),
                                Node(Leaf('V'),
                                     Node(Node(Node(Leaf('Z'),
                                                    Leaf('Q')),
                                               Node(Leaf('X'),
                                                    Leaf('J'))),
                                          Leaf('K')))),
                           Node(Leaf('W'),
                                Leaf('M'))),
                      Leaf('T')),
                 Node(Node(Node(Leaf('C'),
                                Leaf('U')),
                           Leaf('R')),
                      Node(Leaf('H'),
                           Leaf('S')))),
            Node(Node(Node(Node(Node(Leaf('B'),
                                     Leaf('P')),
                                Node(Leaf('Y'),
                                     Leaf('G'))),
                           Leaf('O')),
                      Node(Node(Leaf('L'),
                                Leaf('D')),
                           Leaf('A'))),
                 Node(Leaf('E'),
                      Node(Leaf('N'),
                          Leaf('I')))))  
    
    //the recursive function implode 
    //that put all the character elements in the character
    //list together to turn it into a string
    let rec implode cL =
        match cL with
        |[] -> ""
        |h::t -> (new string(h,1)) + (implode t)
    
    //decoder needs to read & reduce the binary digit list 
    //through trickling down the tree to see
    //if it is a Leaf or Node. If it is a leaf, add it to
    //the character list. If it is a node, keep checking to see 
    //the direction to trickle down the huffman tree (left or right) 
    //until reaching the leaf. When one reaches the end
    //of the given binary character list, return the built up 
    //character list of associated alphabet letters to 
    //the given binary character list.
    let rec decoder h l cL = 
        match h with
        |Leaf(c) -> decoder huffmanTree l (cL@[c])
        |Node(left, right) -> match l with
                              |[] -> cL
                              |h::t -> if h = '0' then decoder left t cL
                                       else decoder right t cL            
    
    //decode accepts a string of binary numbers 
    //then explode it into list of '0' or '1' characters
    //Call the decoder to do the rest of the works while
    //feeding huffmanTree and the exploded list to the decoder.
    let decode (s:string) = 
        decoder huffmanTree (explode s) [] 
