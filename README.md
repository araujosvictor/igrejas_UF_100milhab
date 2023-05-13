# igrejas_UF_100milhab
### Download e replicação dos dados apresentados na nota técnica (Araújo, 2023): Surgimento, trajetória e expansão das Igrejas Evangélicas no território Brasileiro ao longo do último século (1920-2019)

Neste repositório podem ser acessados os arquivos base que permitem a replicação da classificação do universo dos estabelecimentos religiosos registrados na Receita Federal do Brasil. Algumas informações importantes:
  1. O arquivo "df_igrejas_nomes.CSV" é o arquivo base que contém mais de 150 mil estabelecimentos religiosos inscritos na Receita Federal e que se encontravam ativos em 2019, data da coleta das informações;
  2. Os dados de sócios das empresas brasileiras disponíveis no site da Receita Federal foram compilados pela Brasil.io e posteriormente extraídos e organizados pela equipe de cientistas de dados da DADOSCOPE. Do universo de 50 milhões de empresas, agrupadas de acordo com a sua “Classificação Nacional de Atividades Econômicas” (CNAE)”, foram selecionadas apenas aquelas enquadradas na categoria “Atividades de organizações religiosas ou filosóficas” (código CNAE 94.91-0-00), precisamente, 152.142 estabelecimentos religiosos;
  3. O arquivo "codigo_aberto_classificacao.R" disponibiliza o código escrito na linguagem R utilizado na classificação dos estabelecimentos religiosos; 
  4. O arquivo "df_igrejas_UF_medidas.csv" apresenta o resultado da classificação, para quatro grupos evangélicos distintos (Igrejas Evangélicas Missionárias, Pentecostais, Neopentecostais e de Classificação Não Determinada), em cada uma das 27 Unidades Federativas (UFs) brasileiras, entre 1960 e 2019. Este arquivo pode ser baixado e utilizado para fins não comerciais por outros pesquisadores desde que a fonte dos dados seja citada;
  5. Para a construção da medida por 100 mill habitantes e para os testes de validação e consistência, os arquivos "estimativas_pop.csv" e "evangelicos_censo2010.csv" devem ser salvos no mesmo diretório utilizado para as análises no R; 
  6. Para mais informações sobre os dados e os procedimentos utilizados na classificação semi-supervisionada dos estabelecimentos religiosos evangélicos, acesse a Nota técnica original (NT20) publicada na série "Políticas Públicas, Cidades e Desigualdades”, do Centro de Estudos da Metrópole (CEM-Cepid/Fapesp). 

 
