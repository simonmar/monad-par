



all: plot_scaling.exe

plot_scaling.exe: plot_scaling.hs
	ghc --make plot_scaling.hs -o plot_scaling.exe

# Download the (bulkier) full run logs from the web:
getlogs: 
	wget ???

plot:
	./plot_ALL.hs

wipeplots:
#	ls "*/*/*.summary" 
#	ls "*/*/*_graphs"
	find -type f -name "*.summary" -exec rm {} \;
#	find -type d -name "*_graphs" -exec rm -r "{}" \;
	find -type d -name "*_graphs" | xargs -i rm -r {} 

clean:
	rm -f *.hi *.o *.exe


