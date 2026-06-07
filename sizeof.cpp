#include <cstdio>

class Empty {};

class EmptyWithVirt {
	private: virtual void DoTheNeedful() {}
};

class HasInt {
	private: int thing = 0;
};

class HasIntWithVirt {
	private: int thing = 0;
	private: virtual void DoTheNeedful() {}
};

int main(int argc, char **argv) {
	printf("Empty: %d\n", (int)sizeof(Empty));
	printf("EmptyWithVirt: %d\n", (int)sizeof(EmptyWithVirt));
	printf("HasInt: %d\n", (int)sizeof(HasInt));
	printf("HasIntWithVirt: %d\n", (int)sizeof(HasIntWithVirt));
	return 0;
}
