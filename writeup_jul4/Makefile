ts := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")

project = remote_work

package_list.txt:
	 find ../analysis/ -name *R -print0 | xargs -0 grep -h 'library*' | sed 's/library//g' | sed 's/#//g' | sed 's/ //g' | sort | uniq| sed 's/(//g' | sed 's/)//g' > package_list.txt

install_packages: package_list.txt
	cat package_list.txt | xargs -I {} Rscript -e 'install.packages("{}")'

# hacky workaround until I refactor eda.R
media.txt: ../analysis/eda.R
	chmod +x ../analysis/eda.R
	cd ../analysis && ./eda.R
	touch media.txt

$(project).pdf: $(project).bib $(project).tex media.txt
	pdflatex -shell-escape -interaction=nonstopmode $(project)
	bibtex $(project)
	pdflatex -shell-escape -interaction=nonstopmode $(project)
	pdflatex -shell-escape -interaction=nonstopmode $(project)

../backups/$(project)_$(ts).tar.gz:
	tar --exclude='../backups' --exclude='../.git' --exclude='../etl/raw/unused' --exclude='../etl/transformed/unused' --exclude='../unused' --exclude='../etl/robin/' -zcvf ../backups/$(project)_$(ts).tar.gz ../

backup_cloud: ../backups/$(project)_$(ts).tar.gz
	scp ../backups/$(project)_$(ts).tar.gz cloud:/shares/NAS

snapshot:
	@echo Timestamp is $(ts)
	cp $(project).pdf ../snapshots/$(project)_$(ts).pdf

website: $(project).pdf
	printf 'cd papers\nput $<\n' \ | sftp -oPort=2222 johnjhorton@104.198.96.220

$(project).docx:  $(project).bib $(project).tex 
	rm -f $(project).docx
	sed 's/\\input{tables\/\(.*\)\.tex}/\\includegraphics{tables\/\1.png}/' $(project).tex > $(project)_tables_as_images.tex
	#cd tables && sed 's/tables\///' ui.tex | xargs -I{}
	cd tables && texscrap -f ui.tex  --png
	pandoc $(project)_tables_as_images.tex -f latex -t docx -s -o $(project).docx

backup_s3: ../backups/$(project)_$(ts).tar.gz
	aws s3 cp ../backups/$(project)_$(ts).tar.gz s3://john_horton_backups/

toread: $(project).pdf
	scp $(project).pdf bandit-local.local:~/Dropbox/toread/$(project)_$(ts).pdf

purge:
	cd plots && mv *pdf *png unused/
	cd tables && mv *tex unused/

clean:
	rm *.aux *.log *.out *.bbl *.blg *.dvi *.aux *.fff *.lof *.out 
