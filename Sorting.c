#include <stdio.h>

void insertAscending(int a[], int par)
{
  for(int i = par; i > 0; i = i - 1)
  {
    if(a[i] <  a[i - 1])
    {
      int temp = a[i];
      a[i] = a[i - 1];
      a[i - 1] = temp;
    } 
    else
      return;
  } 
}  

void insertionSort(int a[], int length)
{
  for(int par = 0; par < length - 1; par = par + 1)
  {
    insertAscending(a, par + 1);
  }  
}  



int main(int argc, char** argv)
{
  int a[] = {1, 5, -10, 44, 99, 2};
  insertionSort(a, 6);
  for(int i = 0; i < 6; i = i + 1)
  {
    printf("%d ", a[i]);
  } 
  
  return 0; 
}  

extern void __VERIFIER_error() __attribute__ ((__noreturn__));

int xmain()
{
  int a[1000];
  int par = 500;
  int i = par;
  while(i > 0)
  {
    if(a[i] < a[i - 1])
    {
      int temp = a[i];
      a[i] = a[i - 1];
      a[i - 1] = temp;
    } 
    else
      break;
    i = i - 1;
  }  
  
  __VERIFIER_assert(a[i] > a[i - 1]);
  
  return 0;
}  


 
  