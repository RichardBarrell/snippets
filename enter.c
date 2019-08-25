__attribute__((noinline)) int a(int x)
{
	asm volatile("" ::: "memory");
	return x + 1;
}

int main() {
	for (int i=0; i<(100*1000*1000); i++) {
		a(40);
	}
	return 0;
}
