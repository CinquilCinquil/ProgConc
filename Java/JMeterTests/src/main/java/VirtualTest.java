import Virtualv.Main;
import Virtualv.Models.BM25;
import Virtualv.Models.DocumentData;
import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;

import java.io.Serializable;

public class VirtualTest extends AbstractJavaSamplerClient implements Serializable {
    @Override public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        String param_main_n_threads = javaSamplerContext.getParameter("main_n_threads");
        String param_bm25_n_threads = javaSamplerContext.getParameter("bm25_n_threads");
        String param_docdata_n_threads = javaSamplerContext.getParameter("docdata_n_threads");
        String param_docdata_block_size = javaSamplerContext.getParameter("docdata_block_size");
        String param_query = javaSamplerContext.getParameter("query");
        String param_path = javaSamplerContext.getParameter("path");

        SampleResult result = new SampleResult();
        result.sampleStart();
        result.setSampleLabel("Virtual Test Sample");

        Main.n_threads = Integer.parseInt(param_main_n_threads);
        Main.query_text = param_query;
        Main.path = param_path;
        BM25.n_threads = Integer.parseInt(param_bm25_n_threads);
        DocumentData.n_threads = Integer.parseInt(param_docdata_n_threads);
        DocumentData.block_size = Integer.parseInt(param_docdata_block_size);

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
        defaultParameters.addArgument("main_n_threads", String.valueOf(MetaInfo.main_n_threads));
        defaultParameters.addArgument("bm25_n_threads", String.valueOf(MetaInfo.bm25_n_threads));
        defaultParameters.addArgument("docdata_n_threads", String.valueOf(MetaInfo.docdata_n_threads));
        defaultParameters.addArgument("docdata_block_size", String.valueOf(MetaInfo.docdata_block_size));
        defaultParameters.addArgument("query", MetaInfo.query_text);
        defaultParameters.addArgument("path", MetaInfo.path);
        return defaultParameters;
    }
}