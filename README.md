# 6BSGI_Deere_ESALQ
Repositório com dados e produtos gerados para o desafio da John Deere/ESALQ no [6º Workshop de Soluções Matemáticas para Problemas Industriais - CeMEAI - USP](http://www.cemeai.icmc.usp.br/6WSMPI/)

### Códigos
1. O arquivo `rf.R` é um pipeline que modela os dados via RandomForest sobre a latitude e longitude, avalia métricas de erro sobre o conjunto de teste e gera espacializações simples.
2. O arquivo `mapas.R` é um pipeline que gera espacializações a partir dos dadosgerados pelas estratégias usadas pelos demais pesquisadores.

### Pastas
- `dados`: dados de entrada para ambos os scripts
- `mapas`: imagens das espacializações geradas (com base nos dados georreferenciados)
- `shapefile`: arquivos com a máscara para a região de estudo passados pela John Deere/ESALQ

**Apenas os dados de `Ca` foram considerados nestes estudos.**
