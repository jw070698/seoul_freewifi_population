# **서울생활인구를 바탕으로 서울시의 공공와이파이 설치가 필요한 자치구 탐색** <br/>
## 1. 개요
- 프로젝트 소개 및 목표
  - 목표 : 서울의 자치구별 서울생활인구수를 통한 추가적인 공공와이파이 설치 필요 지역 탐색
  - 과학기술정보통신부는 디지털 뉴딜 정책의 일환으로 국민들이 주로 이용하는 공공장소에 20년 말까지 무료 와이파이 1만개소를 추가 확대 구축한다고 발표했다. 특히 21년부터는 국민들이 주로 이용하는 실외생활시설까지 확대 구축하여 무료 데이터 세상을 통해 국민 생활 편의성을 증진할 계획이다. 다음과 같은 계획을 바탕으로 서울시  주민들의 효율적인 무료 와이파이 사용을 위해 서울시 공공와이파이 서비스 위치 정보 데이터와 서울생활인구수 데이터를 통한 공공와이파이 추가 설치 장소를 모색한다.
## 2. 데이터
서울시에서 제공하는 데이터 활용
- 서울시 공공와이파이 서비스 위치 정보 : http://data.seoul.go.kr/dataList/OA-20883/S/1/datasetView.do
  - 사용 항목 : 자치구, 설치년도, x좌표, y좌표
- 행정동별 서울생활인구(내국인) : http://data.seoul.go.kr/dataList/OA-14991/S/1/datasetView.do
  - 사용 항목 : 기준일id, 행정동코드, 총생활인구수
- (참고자료) 자치구단위 서울생활인구(내국인) : https://data.seoul.go.kr/dataList/OA-15439/S/1/datasetView.do
  - 사용 항목 : 2019, 2020, 2021년의 자치구별 집계 정보
  - SeoulPopulation19-21.R : 2019-2021년 정보 추출 과정
  - wifi_pop.rda : 2019-2021년 정보 저장
## 3. 프로젝트 설명
- step1 : 공공데이터 포털 api 스크래핑
  - 서울시 공공와이파이 서비스 위치 정보
    - SeoulWifi_Scrapping.R : 서울시 공공와이파이 서비스 위치 정보 스크래핑
    - SeoulWifi.csv : 스크래핑되어 저장된 데이터
  - 행정동별 서울생활인구(내국인)
    - Seoulpopulation_Scrapping.R : 행정동별 서울생활인구(내국인) 스크래핑
    - Seoulpopulation.csv : 스크래핑되어 저장된 데이터 

- step2 : 공공데이터 기초 탐색
  - 서울시 공공와이파이 서비스 위치 정보
    - SeoulWifi_EDA.R : 서울시 공공와이파이 서비스 위치 정보 EDA 탐색 과정
  - 행정동별 서울생활인구(내국인)
    - seoulpopulation_EDA.R : 행정동별 서울생활인구(내국인) EDA 탐색 과정

- step3 : 대시보드 생성
  - Wifi_map_merge.rda, gu_count.rda, gu_name.csv : 대시보드 파일 로드 과정에 필요
  - TL_SCCO_SIG~ : 대시보드 패키지 실행에 필요
  - DashBoard_new.R 
    1. 서울시의 자치구별 공공와이파이 설치 현황을 나타낸 지도</br>
    <img src="https://user-images.githubusercontent.com/75953480/144628706-f7e3bea1-2baa-41c1-bfd0-edb213c158c8.png" width="400" height="250"/></br>
    2. 서울시의 자치구별 연도에 따른 공공와이파이 개수 현황을 나타낸 막대그래프(2019년, 2020년, 2021년)</br>
    <img src="https://user-images.githubusercontent.com/75953480/144721926-4444a5d5-71a8-4f58-b1c1-c86f192c130e.jpeg" width="600" height="150"/></br>
    <img src="https://user-images.githubusercontent.com/75953480/144721942-6123379c-3ae3-44f9-8075-e1063e86b4d6.jpeg" width="600" height="150"/></br>
    <img src="https://user-images.githubusercontent.com/75953480/144721945-52e14cec-4ffd-428a-980a-a4742f5bf1d9.jpeg" width="600" height="150"/></br>
    3. 서울시의 자치구별 총생활인구수를 나타낸 워드클라우드</br>
    <img src="https://user-images.githubusercontent.com/75953480/144702545-54d28f90-1f60-400c-ae91-8c87f7d17462.png" width="250" height="250"/></br>
    4. 서울시의 자치구별 총생활인구수를 나타낸 트리맵</br>
    <img src="https://user-images.githubusercontent.com/75953480/144702532-1638f9d3-736c-4105-b686-c2ca25b74af1.jpeg" width="550" height="250"/></br>
    5. 서울시의 자치구별 연도에 따른 총생활인구수 현황을 나타낸 막대그래프(2019년, 2020년, 2021년)</br>
    <img src="https://user-images.githubusercontent.com/75953480/144723092-95165157-a410-43dd-a5c5-0f9d8dcc73b6.jpeg" width="600" height="250"/></br>
    <img src="https://user-images.githubusercontent.com/75953480/144723095-e21fb68b-9c94-4fc6-87dd-39d2cb099d30.jpeg" width="600" height="250"/></br>
    <img src="https://user-images.githubusercontent.com/75953480/144723097-21b1712b-06a2-4e2c-b646-65aee1bee947.jpeg" width="600" height="250"/></br>
    6. 서울시의 자치구별 공공와이파이 개수와 서울생활인구의 비율을 나타낸 막대그래프</br>
    <img src="https://user-images.githubusercontent.com/75953480/144721693-b7e99609-4573-40be-b484-f6ee17d366c8.jpeg" width="550" height="250"/></br>
    7. 2021년 11월 서울시의 자치구별 총생활인구수 현황</br>
    <img src="https://user-images.githubusercontent.com/75953480/144724546-88fb3ef8-6cb3-45cf-81df-8f3dc0b57964.jpeg" width="600" height="150"/></br>
## 4. 결론

## 5. 아쉬운 점 및 Future job
- 공공데이터에 최신 데이터가 추가된다면 더 정확한 분석이 가능할 것으로 기대한다.
- 서울생활인구 데이터 사용에 너무 많은 시간이 걸려 기간을 축소하여 사용하였는데, 기간을 확장하여 분석하면 더 좋은 결과를 낼 수 있을 것이다.
## 6. 개발 환경
- R Studio
- R Shiny
