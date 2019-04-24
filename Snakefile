

rule all:
    input:
         "data/su-b-02.02-n-as-gde-17.xlsx", "data/SHAPEFILE_LV95_LN02.zip", "data/su-d-01.02.04.07.xlsx"

rule download_land_cover:
  output:
      "data/su-b-02.02-n-as-gde-17.xlsx"
  shell:
      "curl -L https://www.bfs.admin.ch/bfsstatic/dam/assets/6646409/master > {output}"

rule download_boundaries:
    output:
        "data/SHAPEFILE_LV95_LN02.zip"
    shell:
       """
       curl -L http://data.geo.admin.ch/ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/data.zip > data/temp.zip
       cd data
       unzip temp.zip SHAPEFILE_LV95_LN02.zip
       """
rule download_population:
  output:
      "data/su-d-01.02.04.07.xlsx"
  shell:
      "curl -L https://www.bfs.admin.ch/bfsstatic/dam/assets/5886193/master  > {output}"
