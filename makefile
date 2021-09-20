make:
	f2py3 -c --f90flags='-Wall' --opt='-O3' --verbose RandLib.f90 -m RandLib

clean:
	rm *.mod
