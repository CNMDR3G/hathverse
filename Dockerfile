# HOWTO download:
# $ docker pull scturtle/hathverse
FROM fpco/stack-build:lts-5.11
MAINTAINER scturtle <scturtle@gmail.com>
# setup ~/.stack/global-project/
RUN stack setup
# add extra package: IOSpec
RUN sed -i -e "s/extra-deps: \[\]/extra-deps:\n- lazysmallcheck-0.6\n- Stream-0.4.7.2\n- IOSpec-0.3/g" ~/.stack/global-project/stack.yaml
RUN stack install hspec IOSpec
# build with:
# $ docker build -t scturtle/hathverse .
# $ docker push scturtle/hathverse
