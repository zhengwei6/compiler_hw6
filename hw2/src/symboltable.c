#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<math.h>
#include"header.h"

#define TABLE_SIZE  256

symtab * hash_table[TABLE_SIZE];
extern int linenumber;

int HASH(char * str){
  int idx=0;
  while(*str){
    idx = idx << 1;
    idx+=*str;
    str++;
  }  
  return (idx & (TABLE_SIZE-1));
}

/*returns the symbol table entry if found else NULL*/

symtab * lookup(char *name){
  int hash_key;
  symtab* symptr;
  if(!name)
    return NULL;
  hash_key=HASH(name);
  symptr=hash_table[hash_key];

  while(symptr){
    if(!(strcmp(name,symptr->lexeme)))
      return symptr;
    symptr=symptr->front;
  }
  return NULL;
}


void insertID(char *name){
  int hash_key;
  symtab* ptr;
  symtab* symptr=(symtab*)malloc(sizeof(symtab));  

  hash_key=HASH(name);
  ptr=hash_table[hash_key];

  if(ptr==NULL){
    /*first entry for this hash_key*/
    hash_table[hash_key]=symptr;
    symptr->front=NULL;
    symptr->back=symptr;
  }
  else{
    symptr->front=ptr;
    ptr->back=symptr;
    symptr->back=symptr;
    hash_table[hash_key]=symptr;  
  }

  strcpy(symptr->lexeme,name);
  symptr->line=linenumber;
  symptr->counter=1;
}

char* reservedWord[] = {"return", "typedef", "if", "else", "int", "float", "for", "void", "while"};
#define reservedWordLen 9
int isReserved(const char* name)
{
  int i;
  for (i=0; i<reservedWordLen; ++i)
    if (!strcmp(name, reservedWord[i]))
      return 1;
  return 0;
}

int compareSymtab (const void * a, const void * b)
{
  return strcmp((*(symtab**)a)->lexeme, (*(symtab**)b)->lexeme);
}

void printSym(symtab* ptr) 
{
  printf("%s\t%d\n", ptr->lexeme, ptr->counter);
}

void printSymTab()
{
  int i,j;
  size_t symTabLen = 0;
  symtab* symptr;
  symtab** symbols;
  printf("Frequency of identifiers:\n");
  for (i=0; i<TABLE_SIZE; i++)
  {
    symptr = hash_table[i];
    while (symptr != NULL)
    {
      if (!isReserved(symptr->lexeme))
        ++symTabLen;
      symptr=symptr->front;
    }
  }
  symbols = (symtab**) malloc(symTabLen * sizeof(symtab*));
  for (i=0,j=0; i<TABLE_SIZE; i++)
  {
    symptr = hash_table[i];
    while (symptr != NULL)
    {
      if (!isReserved(symptr->lexeme))
        symbols[j++] = symptr;
      symptr=symptr->front;
    }
  }
  qsort(symbols, symTabLen, sizeof(symtab*), compareSymtab);
  for (i=0; i<symTabLen; i++)
    printSym(symbols[i]);
  free(symbols);
}
