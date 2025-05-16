import Serialv.Main;
import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;

import java.io.Serializable;

public class SerialTest extends AbstractJavaSamplerClient implements Serializable {
    @Override public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        String param_query = javaSamplerContext.getParameter("query");
        String param_path = javaSamplerContext.getParameter("path");

        SampleResult result = new SampleResult();
        result.sampleStart();
        result.setSampleLabel("Serial Test Sample");

        Main.query_text = param_query;
        Main.path = param_path;

        BM25v.Main.main(null);

        if (BM25v.Main.total_processed_docs == 0) {
            result.sampleEnd();
            result.setResponseCode("500");
            result.setResponseMessage("NOK " + Main.total_processed_docs);
            result.setSuccessful(false);
        }
        else {
            result.sampleEnd();
            result.setResponseCode("200");
            result.setResponseMessage("OK " + Main.total_processed_docs);
            result.setSuccessful(true);
        }

        return result;
    }

    @Override public Arguments getDefaultParameters() {
        Arguments defaultParameters = new Arguments();
        defaultParameters.addArgument("query", MetaInfo.query_text);
        defaultParameters.addArgument("path", MetaInfo.path);
        return defaultParameters;
    }
}