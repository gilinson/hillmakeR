/* Update Array */
void updateArray(int *size, double *starts, double *stops, double *returnX, int *countlast)
{
   int i;
   int n = size[0];
    
   for(i = 0; i < n; i++){
    int retIndex;
    for(retIndex = (int)starts[i]; retIndex <= ((int)stops[i] - countlast[0]); retIndex++){
    	returnX[retIndex] = returnX[retIndex] + 1;
    	}
    }
}