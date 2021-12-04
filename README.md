# seoul_freewifi_population<br/>
## 1. 개요
- 프로젝트 소개 및 목표
  - 목표 : 서울의 자치구별 서울생활인구수를 통한 추가적인 공공와이파이 설치 필요 지역 탐색
  - 과학기술정보통신부는 디지털 뉴딜 정책의 일환으로 국민들이 주로 이용하는 공공장소에 20년 말까지 무료 와이파이 1만개소를 추가 확대 구축한다고 발표했다. 특히 21년부터는 국민들이 주로 이용하는 실외생활시설까지 확대 구축하여 무료 데이터 세상을 통해 국민 생활 편의성을 증진할 계획이다. 다음과 같은 계획을 바탕으로 서울시  주민들의 효율적인 무료 와이파이 사용을 위해 서울시 공공와이파이 서비스 위치 정보 데이터와 서울생활인구수 데이터를 통한 공공와이파이 추가 설치 장소를 모색한다.
## 2. 데이터
서울시에서 제공하는 데이터를 활용
- 서울시 공공와이파이 서비스 위치 정보 : http://data.seoul.go.kr/dataList/OA-20883/S/1/datasetView.do
  - 사용 항목 : 자치구, 설치년도, x좌표, y좌표
- 행정동별 서울생활인구(내국인) : http://data.seoul.go.kr/dataList/OA-14991/S/1/datasetView.do
  - 사용 항목 : 기준일id, 행정동코드, 총생활인구수
## 3. 프로젝트 설명
- step1 : 공공데이터 포털 api 스크래핑
  - 서울시 공공와이파이 서비스 위치 정보
    - SeoulWifi_Scrapping.R : 서울시 공공와이파이 서비스 위치 정보 스크래핑
    - SeoulWifi.csv : 스크래핑되어 저장된 데이터

- step2 : 공공데이터 기초 탐색
  - 서울시 공공와이파이 서비스 위치 정보
    - SeoulWifi_EDA.R :   

- step3 : 대시보드 생성
  - DashBoard.R 
    1. 지도 : 서울시의 자치구별 공공와이파이 설치 현황</br>
    <img src="https://user-images.githubusercontent.com/75953480/144628706-f7e3bea1-2baa-41c1-bfd0-edb213c158c8.png" width="400" height="250"/></br>
    2. 시계열 : 서울시의 자치구별 연도에 따른 공공와이파이 개수 현황 
    3. 히트맵 : 서울시 공공와이파이 서비스 위치 정보와 행정동별 서울생활인구의 연관성
    4. 워드클라우드 : 서울시의 자치구별 총생활인구수</br>
    <img src="https://user-images.githubusercontent.com/75953480/144702545-54d28f90-1f60-400c-ae91-8c87f7d17462.png" width="300" height="300"/></br>
    5. 트리맵 : 서울시의 자치구별 총생활인구수</br>
    <img src="https://user-images.githubusercontent.com/75953480/144702532-1638f9d3-736c-4105-b686-c2ca25b74af1.jpeg" width="500" height="250"></br>
    6. 막대그래프 : 서울시의 자치구별 공공와이파이 개수와 서울생활인구의 비율
## 4. 아쉬운 점 및 Future job
- 공공데이터에 최신 데이터가 추가된다면 더 정확한 분석이 가능할 것으로 기대한다.
- 서울생활인구 데이터 사용에 너무 많은 시간이 걸려 기간을 축소하여 사용하였는데, 전체 기간의 데이터를 사용하면 더 좋은 결과를 낼 수 있을 것이다.
## 5. 개발 환경
- R Studio
- R Shiny
