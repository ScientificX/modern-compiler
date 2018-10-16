// compile with: cc -O3 -S add.c

int h(int c) { return 0;}
void g() { };
int f(int a) { int b; b=a+1; g(); h(b); return b+2;}
int main() { f(2); return 0; }
